// =========================================================================
// typcas v2 Differentiation
// =========================================================================

#import "../expr.typ": *
#import "../simplify.typ": simplify
#import "../core/expr-walk.typ": contains-var as _contains-var-core
#import "../truths/function-registry.typ": fn-spec

#let _contains-var(expr, v) = _contains-var-core(expr, v)

#let diff(expr, v) = {
  if is-type(expr, "num") or is-type(expr, "const") { return num(0) }
  if is-type(expr, "var") { return if expr.name == v { num(1) } else { num(0) } }

  if is-type(expr, "neg") {
    return simplify(neg(diff(expr.arg, v)))
  }

  if is-type(expr, "add") {
    return simplify(add(diff(expr.args.at(0), v), diff(expr.args.at(1), v)))
  }

  if is-type(expr, "mul") {
    let f = expr.args.at(0)
    let g = expr.args.at(1)
    return simplify(add(mul(diff(f, v), g), mul(f, diff(g, v))))
  }

  if is-type(expr, "div") {
    let n = expr.num
    let d = expr.den
    return simplify(cdiv(sub(mul(diff(n, v), d), mul(n, diff(d, v))), pow(d, num(2))))
  }

  if is-type(expr, "pow") {
    let b = expr.base
    let e = expr.exp

    // u^n
    if is-type(e, "num") {
      return simplify(mul(mul(e, pow(b, sub(e, num(1)))), diff(b, v)))
    }
    // a^u
    if is-type(b, "num") {
      return simplify(mul(mul(pow(b, e), ln-of(b)), diff(e, v)))
    }
    // u^v = u^v * (v' ln u + v u'/u)
    return simplify(mul(pow(b, e), add(mul(diff(e, v), ln-of(b)), mul(e, cdiv(diff(b, v), b)))))
  }

  if is-type(expr, "log") {
    // d/dx log_b(u) = d/dx ln(u)/ln(b)
    return simplify(diff(cdiv(ln-of(expr.arg), ln-of(expr.base)), v))
  }

  if is-type(expr, "func") {
    let args = func-args(expr)
    let spec = fn-spec(expr.name)
    if spec != none and spec.calculus != none and spec.calculus.diff != none and args.len() == 1 {
      let u = args.at(0)
      let du = diff(u, v)
      let outer = (spec.calculus.diff)(u)
      if is-type(du, "num") and du.val == 1 { return simplify(outer) }
      return simplify(mul(outer, du))
    }
    return func("diff_" + v, expr)
  }

  if is-type(expr, "sum") {
    if expr.idx == v { return num(0) }
    return csum(diff(expr.body, v), expr.idx, expr.from, expr.to)
  }
  if is-type(expr, "prod") {
    if expr.idx == v { return num(0) }
    return cprod(diff(expr.body, v), expr.idx, expr.from, expr.to)
  }

  if is-type(expr, "integral") {
    if expr.var == v { return expr.expr }
    return (type: "integral", expr: diff(expr.expr, v), var: expr.var)
  }
  if is-type(expr, "def-integral") {
    if expr.var == v { return num(0) }
    return (type: "def-integral", expr: diff(expr.expr, v), var: expr.var, lo: expr.lo, hi: expr.hi)
  }

  if is-type(expr, "matrix") {
    return cmat(expr.rows.map(row => row.map(x => diff(x, v))))
  }
  if is-type(expr, "piecewise") {
    return piecewise(expr.cases.map(c => (diff(c.at(0), v), c.at(1))))
  }
  if is-type(expr, "complex") {
    return (type: "complex", re: diff(expr.re, v), im: diff(expr.im, v))
  }

  func("diff_" + v, expr)
}

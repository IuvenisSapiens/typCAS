// =========================================================================
// typcas v2 Advanced Calculus Helpers
// =========================================================================

#import "../expr.typ": *
#import "../simplify.typ": simplify
#import "../eval-num.typ": eval-expr, substitute
#import "diff.typ": diff
#import "integrate.typ": integrate

#let _contains-var(expr, v) = {
  if is-type(expr, "var") { return expr.name == v }
  if is-type(expr, "num") or is-type(expr, "const") { return false }
  if is-type(expr, "neg") { return _contains-var(expr.arg, v) }
  if is-type(expr, "add") or is-type(expr, "mul") {
    return _contains-var(expr.args.at(0), v) or _contains-var(expr.args.at(1), v)
  }
  if is-type(expr, "pow") { return _contains-var(expr.base, v) or _contains-var(expr.exp, v) }
  if is-type(expr, "div") { return _contains-var(expr.num, v) or _contains-var(expr.den, v) }
  if is-type(expr, "func") {
    for a in func-args(expr) {
      if _contains-var(a, v) { return true }
    }
    return false
  }
  if is-type(expr, "log") { return _contains-var(expr.base, v) or _contains-var(expr.arg, v) }
  false
}

#let _fact(n) = {
  let m = int(n)
  if m <= 1 { return 1 }
  let out = 1
  let i = 2
  while i <= m {
    out *= i
    i += 1
  }
  out
}

#let diff-n(expr, v, n) = {
  let out = expr
  let i = 0
  while i < n {
    out = simplify(diff(out, v))
    i += 1
  }
  out
}

#let definite-integral(expr, v, a, b) = {
  let anti = simplify(integrate(expr, v))
  let av = simplify(substitute(anti, v, a))
  let bv = simplify(substitute(anti, v, b))
  simplify(sub(bv, av))
}

#let taylor(expr, v, x0, n) = {
  let x = cvar(v)
  let out = num(0)
  let k = 0
  while k <= n {
    let dk = diff-n(expr, v, k)
    let c = simplify(cdiv(substitute(dk, v, x0), num(_fact(k))))
    let term = if k == 0 { c } else { mul(c, pow(sub(x, x0), num(k))) }
    out = simplify(add(out, term))
    k += 1
  }
  out
}

#let limit(expr, v, a) = {
  let s = simplify(expr)
  let direct = simplify(substitute(s, v, a))
  if not _contains-var(direct, v) { return direct }

  if is-type(direct, "div") {
    let n0 = simplify(substitute(direct.num, v, a))
    let d0 = simplify(substitute(direct.den, v, a))
    if is-type(n0, "num") and is-type(d0, "num") and n0.val == 0 and d0.val == 0 {
      let n1 = simplify(diff(direct.num, v))
      let d1 = simplify(diff(direct.den, v))
      let l1 = simplify(substitute(cdiv(n1, d1), v, a))
      if not _contains-var(l1, v) { return l1 }
    }
  }

  // Numeric symmetric fallback.
  let av = if is-type(a, "num") { a.val + 0.0 } else { 0.0 }
  let h = 1e-6
  let l = eval-expr(substitute(s, v, num(av - h)), (:))
  let r = eval-expr(substitute(s, v, num(av + h)), (:))
  if l != none and r != none and type(l) != dictionary and type(r) != dictionary {
    return num((l + r) / 2.0)
  }

  (type: "limit", expr: s, var: v, to: a)
}

#let implicit-diff(expr, x, y) = {
  // For F(x,y)=0, dy/dx = -Fx/Fy
  let fx = simplify(diff(expr, x))
  let fy = simplify(diff(expr, y))
  simplify(neg(cdiv(fx, fy)))
}

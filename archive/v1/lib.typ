// =========================================================================
// typcas v1 (archived)
// =========================================================================
// Archived v1 entrypoint kept for reference/testing under `archive/v1`.
// This file intentionally preserves the historical v1-style top-level API.
// =========================================================================

#import "src/expr.typ": *
#import "src/parse.typ": cas-parse
#import "src/display.typ": cas-display, cas-equation
#import "src/simplify.typ": simplify as _simplify-core, simplify-meta-core, expand as _expand-core
#import "src/eval-num.typ": eval-expr as _eval-core, substitute as _substitute-core
#import "src/calculus.typ": diff as _diff-core, integrate as _integrate-core, diff-n as _diff-n-core, definite-integral as _definite-integral-core, taylor as _taylor-core, limit as _limit-core, implicit-diff as _implicit-diff-core
#import "src/solve.typ": solve as _solve-core, solve-meta as _solve-meta-core, factor as _factor-core
#import "src/assumptions.typ": assume, assume-domain, assume-string, merge-assumptions, apply-assumptions
#import "src/domain.typ": parse-domain, domain-normalize, domain-intersect, domain-contains, domain-status-rel
#import "src/restrictions.typ": mk-restriction, restriction-key, merge-restrictions, collect-structural-restrictions, collect-function-restrictions, filter-restrictions-by-assumptions
#import "src/steps.typ": step-diff as _step-diff-core, step-integrate as _step-integrate-core, step-simplify as _step-simplify-core, step-solve as _step-solve-core, display-steps
#import "src/matrix.typ": mat-dims, mat-at, mat-add, mat-sub, mat-scale, mat-mul, mat-transpose, mat-det, mat-inv, mat-solve, mat-eigenvalues, mat-eigenvectors
#import "src/system.typ": solve-linear-system as _solve-linear-system-core, solve-nonlinear-system as _solve-nonlinear-system-core
#import "src/poly.typ": poly-coeffs, coeffs-to-expr, poly-div, poly-gcd, partial-fractions

#let _to-expr(v) = {
  if is-expr(v) { return v }
  if type(v) == int or type(v) == float { return num(v) }
  if type(v) == dictionary and v.at("expr", default: none) != none and is-expr(v.expr) { return v.expr }
  cas-parse(v)
}
#let _to-expr-with-assumptions(v, assumptions) = {
  let e = _to-expr(v)
  if assumptions == none { return e }
  apply-assumptions(e, assumptions)
}

#let _has-int-constant(expr) = {
  if expr == none { return false }
  if is-type(expr, "var") { return expr.name == "C" }
  if is-type(expr, "const") { return expr.name == "C" }
  if is-type(expr, "num") { return false }
  if is-type(expr, "neg") { return _has-int-constant(expr.arg) }
  if is-type(expr, "add") or is-type(expr, "mul") {
    return _has-int-constant(expr.args.at(0)) or _has-int-constant(expr.args.at(1))
  }
  if is-type(expr, "pow") { return _has-int-constant(expr.base) or _has-int-constant(expr.exp) }
  if is-type(expr, "div") { return _has-int-constant(expr.num) or _has-int-constant(expr.den) }
  if is-type(expr, "func") {
    for a in func-args(expr) {
      if _has-int-constant(a) { return true }
    }
    return false
  }
  if is-type(expr, "log") { return _has-int-constant(expr.base) or _has-int-constant(expr.arg) }
  if is-type(expr, "sum") or is-type(expr, "prod") {
    return _has-int-constant(expr.body) or _has-int-constant(expr.from) or _has-int-constant(expr.to)
  }
  if is-type(expr, "integral") { return _has-int-constant(expr.expr) }
  if is-type(expr, "def-integral") {
    return _has-int-constant(expr.expr) or _has-int-constant(expr.lo) or _has-int-constant(expr.hi)
  }
  if is-type(expr, "matrix") {
    for row in expr.rows {
      for c in row {
        if _has-int-constant(c) { return true }
      }
    }
    return false
  }
  if is-type(expr, "piecewise") {
    for case in expr.cases {
      if _has-int-constant(case.at(0)) { return true }
    }
    return false
  }
  false
}

#let _with-int-constant(expr) = {
  if expr == none or _has-int-constant(expr) { return expr }
  add(expr, cvar("C"))
}

#let expand(expr) = _expand-core(_to-expr(expr))

#let simplify(expr, expand: false, assumptions: none, allow-domain-sensitive: false) = {
  let src = _to-expr-with-assumptions(expr, assumptions)
  let work = if expand { _expand-core(src) } else { src }
  _simplify-core(work, allow-domain-sensitive: allow-domain-sensitive)
}

#let simplify-meta(expr, expand: false, assumptions: none, allow-domain-sensitive: false) = {
  let src = _to-expr(expr)
  let work = if expand { _expand-core(src) } else { src }
  simplify-meta-core(work, allow-domain-sensitive: allow-domain-sensitive, assumptions: assumptions)
}

#let diff(expr, var, assumptions: none) = _diff-core(_to-expr-with-assumptions(expr, assumptions), var)
#let integrate(expr, var, assumptions: none) = _with-int-constant(_integrate-core(_to-expr-with-assumptions(expr, assumptions), var))
#let diff-n(expr, var, n, assumptions: none) = _diff-n-core(_to-expr-with-assumptions(expr, assumptions), var, n)
#let definite-integral(expr, var, lo, hi, assumptions: none) = _definite-integral-core(
  _to-expr-with-assumptions(expr, assumptions),
  var,
  _to-expr(lo),
  _to-expr(hi),
)
#let taylor(expr, var, x0, n, assumptions: none) = _taylor-core(_to-expr-with-assumptions(expr, assumptions), var, _to-expr(x0), n)
#let limit(expr, var, to, assumptions: none) = _limit-core(_to-expr-with-assumptions(expr, assumptions), var, _to-expr(to))
#let implicit-diff(expr, x, y, assumptions: none) = _implicit-diff-core(_to-expr-with-assumptions(expr, assumptions), x, y)

#let solve(lhs, rhs, var, assumptions: none) = _solve-core(
  _to-expr-with-assumptions(lhs, assumptions),
  _to-expr(rhs),
  var,
)
#let solve-meta(lhs, rhs, var, assumptions: none) = _solve-meta-core(
  _to-expr-with-assumptions(lhs, assumptions),
  _to-expr(rhs),
  var,
)
#let factor(expr, var, assumptions: none) = _factor-core(_to-expr-with-assumptions(expr, assumptions), var)

#let diff-meta(expr, var, assumptions: none) = {
  let src = _to-expr(expr)
  let out = diff(src, var, assumptions: assumptions)
  let structural = collect-structural-restrictions(src)
  let fnr = collect-function-restrictions(src, stage: "diff")
  let merged = merge-restrictions(structural, fnr)
  let filtered = filter-restrictions-by-assumptions(merged, assumptions)
  (
    expr: out,
    restrictions: filtered.restrictions,
    satisfied: filtered.satisfied,
    conflicts: filtered.conflicts,
    residual: filtered.residual,
    variable-domains: filtered.variable-domains,
  )
}

#let eval-expr(expr, bindings) = _eval-core(_to-expr(expr), bindings)
#let substitute(expr, var-name, replacement) = _substitute-core(_to-expr(expr), var-name, _to-expr(replacement))

#let step-diff(expr, var, depth: none, assumptions: none) = _step-diff-core(_to-expr(expr), var, depth: depth, assumptions: assumptions)
#let step-integrate(expr, var, depth: none, assumptions: none) = _step-integrate-core(_to-expr(expr), var, depth: depth, assumptions: assumptions)
#let step-simplify(expr, depth: none, assumptions: none) = _step-simplify-core(_to-expr(expr), depth: depth, assumptions: assumptions)
#let step-solve(lhs, rhs, var, depth: none) = _step-solve-core(_to-expr(lhs), _to-expr(rhs), var, depth: depth)

#let solve-linear-system(equations, vars) = _solve-linear-system-core(
  equations.map(eq => (_to-expr(eq.at(0)), _to-expr(eq.at(1)))),
  vars,
)
#let solve-nonlinear-system(equations, vars, initial, max-iters: 40, tol: 1e-10) = _solve-nonlinear-system-core(
  equations.map(eq => sub(_to-expr(eq.at(0)), _to-expr(eq.at(1)))),
  vars,
  initial,
  max-iters: max-iters,
  tol: tol,
)

// Optional math operators for convenient content parsing.
#let arccsc = math.op("arccsc")
#let arcsec = math.op("arcsec")
#let arccot = math.op("arccot")
#let csch = math.op("csch")
#let sech = math.op("sech")
#let coth = math.op("coth")
#let arcsinh = math.op("arcsinh")
#let arccosh = math.op("arccosh")
#let arctanh = math.op("arctanh")
#let arccsch = math.op("arccsch")
#let arcsech = math.op("arcsech")
#let arccoth = math.op("arccoth")

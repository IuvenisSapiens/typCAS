// =========================================================================
// typcas Regression Check
// =========================================================================
// Compile this file in CI/local checks.
// It panics if any regression check fails.
// =========================================================================

#import "../lib.typ": *

#let _check(name, ok, got: none, expect: none) = (
  name: name,
  ok: ok,
  got: got,
  expect: expect,
)

#let _check-eq(name, got, expect) = {
  let g = simplify(got)
  let e = simplify(expect)
  _check(name, expr-eq(g, e), got: g, expect: e)
}

#let _has-restriction(restrictions, lhs, rel, rhs) = {
  restrictions.any(r =>
    r.rel == rel and expr-eq(simplify(r.lhs), simplify(lhs)) and expr-eq(simplify(r.rhs), simplify(rhs))
  )
}

#let checks = (
  // Sign/canonical normalization regressions.
  _check-eq(
    "sign-cancel-trig-product",
    simplify("2*cos(x)*(-sin(x)) + 2*cos(x)*sin(x)"),
    cas-parse("0"),
  ),
  _check-eq(
    "mixed-denominator-coeff-extract",
    simplify("x/(2*y) + x/(3*y)"),
    cas-parse("(5/6)*(x/y)"),
  ),
  _check-eq(
    "square-form-canonicalization",
    simplify("(x+y)^2 - (x+y)*(x+y)"),
    cas-parse("0"),
  ),
  _check-eq(
    "abs-square-even-power",
    simplify("abs(x)^2 - x^2"),
    cas-parse("0"),
  ),

  // Log reciprocal reductions.
  _check-eq(
    "log-recip-pair",
    simplify("ln(a/b) + ln(b/a)"),
    cas-parse("0"),
  ),
  _check-eq(
    "log-recip-unit",
    simplify("ln(a) + ln(1/a)"),
    cas-parse("0"),
  ),

  // Domain-sensitive policy behavior.
  _check(
    "domain-sensitive-default-off",
    not expr-eq(simplify("arcsin(sin(x))"), cas-parse("x")),
    got: simplify("arcsin(sin(x))"),
    expect: cas-parse("x"),
  ),
  _check-eq(
    "domain-sensitive-explicit-on",
    simplify("arcsin(sin(x))", allow-domain-sensitive: true),
    cas-parse("x"),
  ),

  // Calculus formula parity.
  _check-eq(
    "diff-log2",
    simplify(diff("log2(x)", "x")),
    cas-parse("1/(x*ln(2))"),
  ),
  _check-eq(
    "diff-arcsec-abs-denom",
    simplify(diff("arcsec(x)", "x")),
    cas-parse("1/(abs(x)*sqrt(x^2-1))"),
  ),
  _check-eq(
    "integrate-sec2-plus-csc2",
    simplify(integrate("sec(x)^2 + csc(x)^2", "x")),
    cas-parse("tan(x) - cot(x) + C"),
  ),

  // Restriction-aware simplify metadata.
  _check-eq(
    "dos-cancel-expression",
    simplify("(x^2 - 1)/(x - 1)"),
    cas-parse("x + 1"),
  ),
  _check(
    "dos-cancel-restriction",
    {
      let meta = simplify-meta("(x^2 - 1)/(x - 1)")
      _has-restriction(meta.satisfied, cas-parse("x - 1"), "!=", num(0))
    },
    got: simplify-meta("(x^2 - 1)/(x - 1)"),
    expect: "x - 1 != 0 promoted and satisfied",
  ),
  _check(
    "ln-and-division-restrictions",
    {
      let meta = simplify-meta("ln(x) + 1/(x - 2)")
      _has-restriction(meta.satisfied, cvar("x"), ">", num(0)) and _has-restriction(meta.satisfied, cas-parse("x - 2"), "!=", num(0))
    },
    got: simplify-meta("ln(x) + 1/(x - 2)"),
    expect: "x > 0 and x - 2 != 0 promoted/satisfied",
  ),
  _check(
    "assumption-filtering-ln",
    {
      let meta = simplify-meta("ln(x)", assumptions: assume("x", positive: true, nonzero: true))
      meta.restrictions.len() == 0 and _has-restriction(meta.satisfied, cvar("x"), ">", num(0))
    },
    got: simplify-meta("ln(x)", assumptions: assume("x", positive: true, nonzero: true)),
    expect: "ln restriction satisfied by assumptions",
  ),
  _check(
    "diff-meta-ln-carries-domain",
    {
      let meta = diff-meta("ln(x)", "x")
      _has-restriction(meta.satisfied, cvar("x"), ">", num(0))
    },
    got: diff-meta("ln(x)", "x"),
    expect: "x > 0 promoted in diff metadata",
  ),
  _check(
    "domain-string-open-positive",
    {
      let meta = simplify-meta("ln(x)", assumptions: assume-domain("x", "(0"))
      meta.restrictions.len() == 0 and _has-restriction(meta.satisfied, cvar("x"), ">", num(0))
    },
    got: simplify-meta("ln(x)", assumptions: assume-domain("x", "(0")),
    expect: "(0 means x > 0",
  ),
  _check(
    "domain-string-closed-nonnegative",
    {
      let meta = simplify-meta("ln(x)", assumptions: assume-domain("x", "[0"))
      _has-restriction(meta.satisfied, cvar("x"), ">", num(0)) and meta.variable-domains.at("x", default: none) != none
    },
    got: simplify-meta("ln(x)", assumptions: assume-domain("x", "[0")),
    expect: "[0 narrowed by ln to x > 0",
  ),
  _check(
    "affine-promotion-ln-x-minus-2",
    {
      let meta = simplify-meta("ln(x - 2)", assumptions: assume-domain("x", "(2,inf)"))
      _has-restriction(meta.satisfied, cas-parse("x - 2"), ">", num(0))
    },
    got: simplify-meta("ln(x - 2)", assumptions: assume-domain("x", "(2,inf)")),
    expect: "x - 2 > 0 satisfied via propagated variable domain",
  ),
  _check(
    "affine-promotion-emits-variable-domain",
    {
      let meta = simplify-meta("ln(2*x - 4)")
      let dx = meta.variable-domains.at("x", default: none)
      dx != none and dx.intervals.len() == 1 and dx.intervals.at(0).lo == 2 and dx.intervals.at(0).at("lo-closed", default: true) == false
    },
    got: simplify-meta("ln(2*x - 4)"),
    expect: "propagated domain x > 2",
  ),
  _check(
    "non-affine-residual-kept",
    {
      let meta = simplify-meta("ln(x + y)")
      meta.residual.len() >= 1 and _has-restriction(meta.residual, cas-parse("x + y"), ">", num(0))
    },
    got: simplify-meta("ln(x + y)"),
    expect: "x + y > 0 remains residual non-affine constraint",
  ),
  _check(
    "parse-domain-valid-union",
    {
      let d = parse-domain("(-inf,-3)U(-3,inf)")
      d.intervals.len() == 2
    },
    got: parse-domain("(-inf,-3)U(-3,inf)"),
    expect: "two intervals",
  ),
  _check(
    "parse-domain-valid-closed-open",
    {
      let d = parse-domain("[-2,-1)")
      d.intervals.len() == 1 and d.intervals.at(0).lo == -2 and d.intervals.at(0).at("lo-closed", default: false)
    },
    got: parse-domain("[-2,-1)"),
    expect: "single interval [-2,-1)",
  ),
  _check(
    "parse-domain-cut-shorthand-single",
    {
      let d = parse-domain("2)(2")
      d.intervals.len() == 2 and d.intervals.at(0).hi == 2 and d.intervals.at(1).lo == 2
    },
    got: parse-domain("2)(2"),
    expect: "(-inf,2)U(2,inf)",
  ),
  _check(
    "parse-domain-cut-shorthand-chain",
    {
      let d = parse-domain("2)(2,3)(3")
      d.intervals.len() == 3 and d.intervals.at(0).hi == 2 and d.intervals.at(1).lo == 2 and d.intervals.at(1).hi == 3 and d.intervals.at(2).lo == 3
    },
    got: parse-domain("2)(2,3)(3"),
    expect: "(-inf,2)U(2,3)U(3,inf)",
  ),
  _check(
    "parse-domain-base-syntax-mixed",
    {
      let d = parse-domain("2)[3,4](5")
      d.intervals.len() == 3 and d.intervals.at(0).hi == 2 and d.intervals.at(1).lo == 3 and d.intervals.at(1).hi == 4 and d.intervals.at(2).lo == 5
    },
    got: parse-domain("2)[3,4](5"),
    expect: "(-inf,2)U[3,4]U(5,inf)",
  ),
  _check(
    "parse-domain-base-syntax-rays",
    {
      let d1 = parse-domain("3)")
      let d2 = parse-domain("(4")
      d1.intervals.len() == 1 and d2.intervals.len() == 1 and d1.intervals.at(0).hi == 3 and d2.intervals.at(0).lo == 4
    },
    got: (parse-domain("3)"), parse-domain("(4")),
    expect: "left and right rays",
  ),

  // Solver sanity.
  _check(
    "cubic-solve-nonempty",
    solve("x^3 + 2*x^2 + 3*x + 1", 0, "x").len() >= 1,
    got: solve("x^3 + 2*x^2 + 3*x + 1", 0, "x"),
    expect: "at least one root",
  ),
)

#let failures = checks.filter(c => not c.ok)

#if failures.len() > 0 {
  panic("Regression failures:\n" + repr(failures))
}

= typcas Regression Check

All regression checks passed: #checks.len().

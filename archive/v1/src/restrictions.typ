// =========================================================================
// CAS Restrictions Engine
// =========================================================================
// Collects and propagates advisory domain restrictions as metadata.
//
// Restriction record shape:
//   (lhs: expr, rel: "!=" | ">" | ">=" | "<" | "<=", rhs: expr,
//    source: str, stage: str, note: str)
//
// This module does not enforce restrictions at runtime. It only reports:
//   - unresolved restrictions
//   - restrictions satisfied by assumptions
//   - restrictions conflicting with assumptions
// =========================================================================

#import "expr.typ": *
#import "display.typ": cas-display
#import "truths/function-registry.typ": fn-spec
#import "domain.typ": domain-status-rel, domain-intersect, domain-normalize

/// Public helper `mk-restriction`.
#let mk-restriction(lhs, rel, rhs, source: "", stage: "defined", note: "") = (
  lhs: lhs,
  rel: rel,
  rhs: rhs,
  source: source,
  stage: stage,
  note: note,
)

/// Public helper `restriction-key`.
/// Dedupe key is structural `(lhs, rel, rhs)` only.
#let restriction-key(r) = r.rel + ":" + repr(r.lhs) + ":" + repr(r.rhs)

/// Public helper `merge-restrictions`.
#let merge-restrictions(a, b) = {
  let out = ()
  let seen = (:)
  for r in a + b {
    let k = restriction-key(r)
    if not seen.at(k, default: false) {
      out.push(r)
      seen.insert(k, true)
    }
  }
  out
}

/// Internal helper `_assume-get`.
#let _assume-get(assumptions, var, key) = {
  let rec = assumptions.at(var, default: (:))
  rec.at(key, default: false)
}

/// Internal helper `_assume-domain`.
#let _assume-domain(assumptions, var) = {
  let rec = assumptions.at(var, default: (:))
  rec.at("domain", default: none)
}

/// Internal helper `_domain-from-rel`.
/// Returns a domain-set for a simple scalar relation `x rel c`.
#let _domain-from-rel(rel, c) = {
  if rel == ">" {
    return domain-normalize((intervals: ((lo: c, lo-closed: false, hi: none, hi-closed: false),)))
  }
  if rel == ">=" {
    return domain-normalize((intervals: ((lo: c, lo-closed: true, hi: none, hi-closed: false),)))
  }
  if rel == "<" {
    return domain-normalize((intervals: ((lo: none, lo-closed: false, hi: c, hi-closed: false),)))
  }
  if rel == "<=" {
    return domain-normalize((intervals: ((lo: none, lo-closed: false, hi: c, hi-closed: true),)))
  }
  if rel == "!=" {
    return domain-normalize((intervals: (
      (lo: none, lo-closed: false, hi: c, hi-closed: false),
      (lo: c, lo-closed: false, hi: none, hi-closed: false),
    )))
  }
  none
}

/// Internal helper `_domain-empty`.
#let _domain-empty(d) = {
  if d == none { return true }
  domain-normalize(d).intervals.len() == 0
}

/// Internal helper `_seed-domain-from-assumption-record`.
#let _seed-domain-from-assumption-record(rec) = {
  let d = rec.at("domain", default: none)
  if rec.at("positive", default: false) {
    let x = _domain-from-rel(">", 0)
    d = if d == none { x } else { domain-intersect(d, x) }
  }
  if rec.at("nonnegative", default: false) {
    let x = _domain-from-rel(">=", 0)
    d = if d == none { x } else { domain-intersect(d, x) }
  }
  if rec.at("negative", default: false) {
    let x = _domain-from-rel("<", 0)
    d = if d == none { x } else { domain-intersect(d, x) }
  }
  if rec.at("nonzero", default: false) {
    let x = _domain-from-rel("!=", 0)
    d = if d == none { x } else { domain-intersect(d, x) }
  }
  if d == none { return none }
  domain-normalize(d)
}

/// Internal helper `_effective-assumptions`.
/// Clones user assumptions and overlays propagated domains per variable.
#let _effective-assumptions(assumptions, vars) = {
  let out = (:)
  if assumptions != none {
    for k in assumptions.keys() {
      out.insert(k, assumptions.at(k))
    }
  }
  for k in vars.keys() {
    let rec = out.at(k, default: (:))
    out.insert(k, (..rec, domain: vars.at(k)))
  }
  out
}

/// Internal helper `_merge-status`.
#let _merge-status(a, b) = {
  if a == "conflict" or b == "conflict" { return "conflict" }
  if a == "satisfied" or b == "satisfied" { return "satisfied" }
  "unknown"
}

/// Internal helper `_status-var-num`.
/// Returns "satisfied" | "conflict" | "unknown".
#let _status-var-num(var-name, rel, c, assumptions) = {
  let pos = _assume-get(assumptions, var-name, "positive")
  let neg = _assume-get(assumptions, var-name, "negative")
  let nonneg = _assume-get(assumptions, var-name, "nonnegative")
  let nonzero = _assume-get(assumptions, var-name, "nonzero") or pos or neg
  let bool-status = "unknown"

  if rel == "!=" and c == 0 {
    if nonzero { bool-status = "satisfied" }
  }

  if rel == ">" {
    if pos and c <= 0 { bool-status = "satisfied" }
    if nonneg and c < 0 { bool-status = "satisfied" }
    if neg and c >= 0 { bool-status = "conflict" }
  }

  if rel == ">=" {
    if pos and c <= 0 { bool-status = "satisfied" }
    if nonneg and c <= 0 { bool-status = "satisfied" }
    if neg and c >= 0 { bool-status = "conflict" }
  }

  if rel == "<" {
    if neg and c > 0 { bool-status = "satisfied" }
    if pos and c <= 0 { bool-status = "conflict" }
    if nonneg and c <= 0 { bool-status = "conflict" }
  }

  if rel == "<=" {
    if neg and c >= 0 { bool-status = "satisfied" }
    if pos and c < 0 { bool-status = "conflict" }
    if nonneg and c < 0 { bool-status = "conflict" }
  }

  let domain-status = {
    let d = _assume-domain(assumptions, var-name)
    if d == none { "unknown" } else { domain-status-rel(d, rel, c) }
  }
  _merge-status(bool-status, domain-status)
}

/// Internal helper `_vars-in-expr`.
#let _vars-in-expr(expr) = {
  let out = ()
  if is-type(expr, "var") {
    out.push(expr.name)
    return out
  }
  if is-type(expr, "num") or is-type(expr, "const") {
    return out
  }
  if is-type(expr, "neg") {
    return _vars-in-expr(expr.arg)
  }
  if is-type(expr, "add") or is-type(expr, "mul") {
    let a = _vars-in-expr(expr.args.at(0))
    let b = _vars-in-expr(expr.args.at(1))
    for n in b {
      if n not in a { a.push(n) }
    }
    return a
  }
  if is-type(expr, "div") {
    let a = _vars-in-expr(expr.num)
    let b = _vars-in-expr(expr.den)
    for n in b {
      if n not in a { a.push(n) }
    }
    return a
  }
  if is-type(expr, "pow") {
    let a = _vars-in-expr(expr.base)
    let b = _vars-in-expr(expr.exp)
    for n in b {
      if n not in a { a.push(n) }
    }
    return a
  }
  if is-type(expr, "func") {
    let acc = ()
    for arg in func-args(expr) {
      let vv = _vars-in-expr(arg)
      for n in vv {
        if n not in acc { acc.push(n) }
      }
    }
    return acc
  }
  if is-type(expr, "log") {
    let a = _vars-in-expr(expr.base)
    let b = _vars-in-expr(expr.arg)
    for n in b {
      if n not in a { a.push(n) }
    }
    return a
  }
  out
}

/// Internal helper `_affine`.
/// Returns `(a, b)` for `a*var + b`, or none.
#let _affine(expr, var) = {
  if is-type(expr, "num") {
    return (a: 0.0, b: expr.val + 0.0)
  }
  if is-type(expr, "var") {
    if expr.name == var {
      return (a: 1.0, b: 0.0)
    }
    return none
  }
  if is-type(expr, "neg") {
    let t = _affine(expr.arg, var)
    if t == none { return none }
    return (a: -t.a, b: -t.b)
  }
  if is-type(expr, "add") {
    let l = _affine(expr.args.at(0), var)
    let r = _affine(expr.args.at(1), var)
    if l == none or r == none { return none }
    return (a: l.a + r.a, b: l.b + r.b)
  }
  if is-type(expr, "mul") {
    let l = _affine(expr.args.at(0), var)
    let r = _affine(expr.args.at(1), var)
    if l == none or r == none { return none }
    if l.a == 0 {
      return (a: l.b * r.a, b: l.b * r.b)
    }
    if r.a == 0 {
      return (a: r.b * l.a, b: r.b * l.b)
    }
    return none
  }
  if is-type(expr, "div") {
    let n = _affine(expr.num, var)
    let d = _affine(expr.den, var)
    if n == none or d == none { return none }
    if d.a != 0 or d.b == 0 { return none }
    return (a: n.a / d.b, b: n.b / d.b)
  }
  none
}

/// Internal helper `_flip-rel`.
#let _flip-rel(rel) = {
  if rel == ">" { return "<" }
  if rel == ">=" { return "<=" }
  if rel == "<" { return ">" }
  if rel == "<=" { return ">=" }
  rel
}

/// Internal helper `_normalize-affine-rel`.
/// Normalizes `lhs rel rhs` into `x rel c` for simple affine forms.
#let _normalize-affine-rel(lhs, rel, rhs) = {
  let vars = _vars-in-expr(sub(lhs, rhs))
  if vars.len() != 1 { return none }
  let v = vars.at(0)
  let ab = _affine(sub(lhs, rhs), v)
  if ab == none or ab.a == 0 { return none }

  let out-rel = if ab.a < 0 { _flip-rel(rel) } else { rel }
  let c = -ab.b / ab.a
  (var: v, rel: out-rel, c: c)
}

/// Internal helper `_promote-constraint`.
/// Converts promotable constraints into scalar variable form `x rel c`.
#let _promote-constraint(r) = {
  let lhs = r.lhs
  let rhs = r.rhs

  if is-type(lhs, "var") and is-type(rhs, "num") {
    return (var: lhs.name, rel: r.rel, c: rhs.val + 0.0)
  }

  // |x| > 0  =>  x != 0
  if is-type(lhs, "func") and lhs.name == "abs" and func-arity(lhs) == 1 and is-type(func-args(lhs).at(0), "var") and is-type(rhs, "num") and r.rel == ">" and rhs.val == 0 {
    return (var: func-args(lhs).at(0).name, rel: "!=", c: 0.0)
  }

  _normalize-affine-rel(lhs, r.rel, rhs)
}

/// Internal helper `_restriction-status-late`.
/// Local copy for propagation (Typst requires callee to be defined earlier).
#let _restriction-status-late(r, assumptions) = {
  if assumptions == none { return "unknown" }
  let lhs = r.lhs
  let rhs = r.rhs

  if is-type(lhs, "var") and is-type(rhs, "num") {
    return _status-var-num(lhs.name, r.rel, rhs.val, assumptions)
  }

  if is-type(lhs, "func") and lhs.name == "abs" and func-arity(lhs) == 1 and is-type(func-args(lhs).at(0), "var") and is-type(rhs, "num") {
    let inner = func-args(lhs).at(0)
    let c = rhs.val
    if r.rel == ">" and c == 0 {
      return _status-var-num(inner.name, "!=", 0, assumptions)
    }
    if r.rel == "<" and c <= 0 { return "conflict" }
  }

  let aff = _normalize-affine-rel(lhs, r.rel, rhs)
  if aff != none {
    return _status-var-num(aff.var, aff.rel, aff.c, assumptions)
  }

  "unknown"
}

/// Public helper `propagate-variable-domains`.
/// Promotes univariate affine constraints into per-variable domains.
#let propagate-variable-domains(constraints, assumptions: none) = {
  let vars = (:)
  if assumptions != none {
    for k in assumptions.keys() {
      let rec = assumptions.at(k, default: (:))
      let d = _seed-domain-from-assumption-record(rec)
      if d != none and not _domain-empty(d) {
        vars.insert(k, d)
      }
    }
  }

  let unique = merge-restrictions((), constraints)
  let residual = ()
  let propagated-conflicts = ()
  let conflict-keys = (:)

  for r in unique {
    let p = _promote-constraint(r)
    if p == none {
      residual.push(r)
      continue
    }

    let d = _domain-from-rel(p.rel, p.c)
    if d == none {
      residual.push(r)
      continue
    }

    let curr = vars.at(p.var, default: none)
    let next = if curr == none { d } else { domain-intersect(curr, d) }
    if _domain-empty(next) {
      propagated-conflicts.push(r)
      conflict-keys.insert(restriction-key(r), true)
      continue
    }
    vars.insert(p.var, domain-normalize(next))
  }

  let eff-assumptions = _effective-assumptions(assumptions, vars)
  let unresolved = ()
  let satisfied = ()
  let conflicts = ()

  for r in unique {
    let k = restriction-key(r)
    if conflict-keys.at(k, default: false) {
      conflicts.push(r)
      continue
    }
    let status = _restriction-status-late(r, eff-assumptions)
    if status == "satisfied" {
      satisfied.push(r)
    } else if status == "conflict" {
      conflicts.push(r)
    } else {
      unresolved.push(r)
    }
  }

  (
    vars: vars,
    residual: residual,
    conflicts: merge-restrictions(conflicts, propagated-conflicts),
    restrictions: unresolved,
    satisfied: satisfied,
    assumptions: eff-assumptions,
  )
}

/// Internal helper `_restriction-status`.
#let _restriction-status(r, assumptions) = {
  if assumptions == none { return "unknown" }
  let lhs = r.lhs
  let rhs = r.rhs

  if is-type(lhs, "var") and is-type(rhs, "num") {
    return _status-var-num(lhs.name, r.rel, rhs.val, assumptions)
  }

  if is-type(lhs, "func") and lhs.name == "abs" and func-arity(lhs) == 1 and is-type(func-args(lhs).at(0), "var") and is-type(rhs, "num") {
    let inner = func-args(lhs).at(0)
    let inner-name = inner.name
    let c = rhs.val

    // |x| > 0 holds if x is known nonzero.
    if r.rel == ">" and c == 0 {
      return _status-var-num(inner-name, "!=", 0, assumptions)
    }

    // |x| < 0 is impossible.
    if r.rel == "<" and c <= 0 { return "conflict" }
  }

  let aff = _normalize-affine-rel(lhs, r.rel, rhs)
  if aff != none {
    return _status-var-num(aff.var, aff.rel, aff.c, assumptions)
  }

  "unknown"
}

/// Public helper `filter-restrictions-by-assumptions`.
#let filter-restrictions-by-assumptions(restrictions, assumptions) = {
  let p = propagate-variable-domains(restrictions, assumptions: assumptions)
  (
    restrictions: p.restrictions,
    satisfied: p.satisfied,
    conflicts: p.conflicts,
    residual: p.residual,
    variable-domains: p.vars,
  )
}

/// Internal helper `_collect-structural`.
#let _collect-structural(expr) = {
  let out = ()

  if is-type(expr, "num") or is-type(expr, "var") or is-type(expr, "const") {
    return out
  }

  if is-type(expr, "neg") {
    return _collect-structural(expr.arg)
  }

  if is-type(expr, "add") or is-type(expr, "mul") {
    return merge-restrictions(
      _collect-structural(expr.args.at(0)),
      _collect-structural(expr.args.at(1)),
    )
  }

  if is-type(expr, "pow") {
    return merge-restrictions(
      _collect-structural(expr.base),
      _collect-structural(expr.exp),
    )
  }

  if is-type(expr, "div") {
    out.push(mk-restriction(
      expr.den,
      "!=",
      num(0),
      source: "structural:division",
      stage: "defined",
      note: "Denominator must be nonzero.",
    ))
    return merge-restrictions(
      out,
      merge-restrictions(_collect-structural(expr.num), _collect-structural(expr.den)),
    )
  }

  if is-type(expr, "func") {
    let acc = ()
    for a in func-args(expr) {
      acc = merge-restrictions(acc, _collect-structural(a))
    }
    return acc
  }

  if is-type(expr, "log") {
    out.push(mk-restriction(
      expr.base,
      ">",
      num(0),
      source: "structural:log",
      stage: "defined",
      note: "Logarithm base must be positive.",
    ))
    out.push(mk-restriction(
      expr.base,
      "!=",
      num(1),
      source: "structural:log",
      stage: "defined",
      note: "Logarithm base cannot be 1.",
    ))
    out.push(mk-restriction(
      expr.arg,
      ">",
      num(0),
      source: "structural:log",
      stage: "defined",
      note: "Logarithm argument must be positive.",
    ))
    let inner = merge-restrictions(_collect-structural(expr.base), _collect-structural(expr.arg))
    return merge-restrictions(out, inner)
  }

  if is-type(expr, "sum") or is-type(expr, "prod") {
    return merge-restrictions(
      _collect-structural(expr.body),
      merge-restrictions(_collect-structural(expr.from), _collect-structural(expr.to)),
    )
  }

  if is-type(expr, "matrix") {
    let acc = ()
    for row in expr.rows {
      for item in row {
        acc = merge-restrictions(acc, _collect-structural(item))
      }
    }
    return acc
  }

  if is-type(expr, "piecewise") {
    let acc = ()
    for (body, cond) in expr.cases {
      acc = merge-restrictions(acc, _collect-structural(body))
      if is-expr(cond) {
        acc = merge-restrictions(acc, _collect-structural(cond))
      }
    }
    return acc
  }

  out
}

/// Public helper `collect-structural-restrictions`.
#let collect-structural-restrictions(expr) = _collect-structural(expr)

/// Internal helper `_collect-function`.
#let _collect-function(expr, stage) = {
  let out = ()

  if is-type(expr, "num") or is-type(expr, "var") or is-type(expr, "const") {
    return out
  }

  if is-type(expr, "neg") {
    return _collect-function(expr.arg, stage)
  }

  if is-type(expr, "add") or is-type(expr, "mul") {
    return merge-restrictions(
      _collect-function(expr.args.at(0), stage),
      _collect-function(expr.args.at(1), stage),
    )
  }

  if is-type(expr, "pow") {
    return merge-restrictions(
      _collect-function(expr.base, stage),
      _collect-function(expr.exp, stage),
    )
  }

  if is-type(expr, "div") {
    return merge-restrictions(
      _collect-function(expr.num, stage),
      _collect-function(expr.den, stage),
    )
  }

  if is-type(expr, "func") {
    let args = func-args(expr)
    let acc = ()
    for a in args {
      acc = merge-restrictions(acc, _collect-function(a, stage))
    }

    let spec = fn-spec(expr.name)
    if spec != none {
      let block = spec.at("restrictions", default: none)
      if block != none {
        let producer = block.at(stage, default: none)
        if producer != none {
          let generated = producer(args)
          if generated != none {
            acc = merge-restrictions(acc, generated)
          }
        }
      }
    }

    return acc
  }

  if is-type(expr, "log") {
    return merge-restrictions(
      _collect-function(expr.base, stage),
      _collect-function(expr.arg, stage),
    )
  }

  if is-type(expr, "sum") or is-type(expr, "prod") {
    return merge-restrictions(
      _collect-function(expr.body, stage),
      merge-restrictions(_collect-function(expr.from, stage), _collect-function(expr.to, stage)),
    )
  }

  if is-type(expr, "matrix") {
    let acc = ()
    for row in expr.rows {
      for item in row {
        acc = merge-restrictions(acc, _collect-function(item, stage))
      }
    }
    return acc
  }

  if is-type(expr, "piecewise") {
    let acc = ()
    for (body, cond) in expr.cases {
      acc = merge-restrictions(acc, _collect-function(body, stage))
      if is-expr(cond) {
        acc = merge-restrictions(acc, _collect-function(cond, stage))
      }
    }
    return acc
  }

  out
}

/// Public helper `collect-function-restrictions`.
#let collect-function-restrictions(expr, stage: "defined") = _collect-function(expr, stage)

/// Internal helper `_restriction-math`.
#let _restriction-math(r) = {
  if r.rel == "!=" { return $#cas-display(r.lhs) != #cas-display(r.rhs)$ }
  if r.rel == ">" { return $#cas-display(r.lhs) > #cas-display(r.rhs)$ }
  if r.rel == ">=" { return $#cas-display(r.lhs) >= #cas-display(r.rhs)$ }
  if r.rel == "<" { return $#cas-display(r.lhs) < #cas-display(r.rhs)$ }
  if r.rel == "<=" { return $#cas-display(r.lhs) <= #cas-display(r.rhs)$ }
  $#cas-display(r.lhs) #r.rel #cas-display(r.rhs)$
}

/// Public helper `render-restriction-note`.
#let render-restriction-note(r) = {
  let note = r.at("note", default: "")
  if note != "" {
    return [
      #text(size: 0.9em, fill: luma(100), style: "italic")[Restriction: ]
      #_restriction-math(r)
      #h(0.5em)
      #text(size: 0.85em, fill: luma(120))[#note]
    ]
  }
  [
    #text(size: 0.9em, fill: luma(100), style: "italic")[Restriction: ]
    #_restriction-math(r)
  ]
}

An implementation of PTS (CoC) in Scala for practice.

## Examples
```
assume False: *;
assume false_elim: forall P: *. False -> P;
define Not: * -> * = fun P: *. P -> False;
assume And: * -> * -> *;
assume and_intro: forall P: *, Q: *. P -> Q -> And P Q;
assume and_elim: forall P: *, Q: *, R: *. And P Q -> (P -> Q -> R) -> R;
define projl: forall P: *, Q: *. And P Q -> P
= fun P: *, Q: *.
  fun H: And P Q.
  and_elim P Q P H (fun H1: P, H2: Q. H1);
define projr: forall P: *, Q: *. And P Q -> Q
= fun P: *, Q: *.
  fun H: And P Q.
  and_elim P Q Q H (fun H1: P, H2: Q. H2);
assume Or: * -> * -> *;
assume or_introl: forall P: *, Q: *. P -> Or P Q;
assume or_intror: forall P: *, Q: *. Q -> Or P Q;
assume or_elim: forall P: *, Q: *, R: *. Or P Q -> (P -> R) -> (Q -> R) -> R;
define Equiv: * -> * -> * = fun P: *, Q: *. And (P -> Q) (Q -> P);
assume Eq: forall T: *. T -> T -> *;
assume eq_axiom: forall T: *, x: T, y: T. Equiv (Eq T x y) (forall P: T -> *. Equiv (P x) (P y));
define eq_refl: forall T: *, x: T. Eq T x x
= fun T: *, x: T.
  projr
    (Eq T x x -> forall P: T -> *. Equiv (P x) (P x))
    ((forall P: T -> *. Equiv (P x) (P x)) -> Eq T x x)
    (eq_axiom T x x)
    (
      fun P: T -> *.
      and_intro
        (P x -> P x)
        (P x -> P x)
        (fun H: P x. H)
        (fun H: P x. H)
    );
define eq_symm: forall T: *, x: T, y: T. Eq T x y -> Eq T y x
= fun T: *, x: T, y: T.
  fun H: Eq T x y.
  projr
    (Eq T y x -> Eq T y y)
    (Eq T y y -> Eq T y x)
    (
      projl
        (Eq T x y -> forall P: T -> *. Equiv (P x) (P y))
        ((forall P: T -> *. Equiv (P x) (P y)) -> Eq T x y)
        (eq_axiom T x y)
        H
        (Eq T y)
    )
    (eq_refl T y);
define eq_trans: forall T: *, x: T, y: T, z: T. Eq T x y -> Eq T y z -> Eq T x z
= fun T: *, x: T, y: T, z: T.
  fun H1: Eq T x y.
  fun H2: Eq T y z.
  projl
    (Eq T x y -> Eq T x z)
    (Eq T x z -> Eq T x y)
    (
      projl
        (Eq T y z -> forall P: T -> *. Equiv (P y) (P z))
        ((forall P: T -> *. Equiv (P y) (P z)) -> Eq T y z)
        (eq_axiom T y z)
        H2
        (Eq T x)
    )
    H1;
define f_equal: forall T: *, x: T, y: T. Eq T x y -> forall U: *, f: T -> U. Eq U (f x) (f y)
= fun T: *, x: T, y: T.
  fun H: Eq T x y.
  fun U: *, f: T -> U.
  projr
    (Eq U (f x) (f y) -> forall P: U -> *. Equiv (P (f x)) (P (f y)))
    ((forall P: U -> *. Equiv (P (f x)) (P (f y))) -> Eq U (f x) (f y))
    (eq_axiom U (f x) (f y))
    (
      fun P: U -> *.
      projl
        (Eq T x y -> forall Q: T -> *. Equiv (Q x) (Q y))
        ((forall Q: T -> *. Equiv (Q x) (Q y)) -> Eq T x y)
        (eq_axiom T x y)
        H
        (fun z: T. P (f z))
    );
```

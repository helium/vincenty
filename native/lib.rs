use rustler::{Encoder, Env, NifResult, Term};

mod vincenty;

rustler::atoms! {
    ok,
    error,
    fail_to_converge,
}

#[rustler::nif(name = "distance")]
pub fn distance(env: Env, c1: vincenty::Coordinate, c2: vincenty::Coordinate) -> NifResult<Term> {
    match vincenty::distance(c1, c2) {
        Some(d) => Ok((ok(), d).encode(env)),
        None => Ok((error(), fail_to_converge()).encode(env)),
    }
}

rustler::init!("vincenty", [distance]);

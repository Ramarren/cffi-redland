# CFFI bindings for [Redland](http://librdf.org/docs/api/index.html) RDF Library.

## Dependencies

- cffi
- iterate
- alexandria
- trivial-garbage

## Overview

The low-level bindings are partially written by hand and parially automatically generated. They cover almost all of the library, although I have not verified that all strings are handled correctly. There might remain some confusion between `new-string`/`:string`/`:pointer`.

Mid level bindings, wrapping all Redland objects with CLOS objects are somewhat complete. They are fairly raw, mostly handling wrapping/unwrapping pointers. They are pretty close to C functions, so Redland API reference can be used as documentation.

There are no mid-level bindings for hashes and lists. CL has pretty good versions of those already.

There are drivers for ITERATE, for Redland streams, iterators, models and query-results. Note that iteration object is shared and has to be copied to be used outside the loop.

Low-level bindings are unexported symbols prefixed with `%`. Mid-level bindings are exported. There are some minimal tests in `test.lisp`.

## Status

I wrote those bindings as part of figuring out what is this RDF thing I saw mentioned, since I find that retyping APIs help me understand things. Unfortunately, I have no project to actually use it in, so it is unlikely that I will do more complete testing or write any higher level lispy framework on top in foreseeable future. I just hope someone might find these a helpful basis.

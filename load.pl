% Non-debug load file for the Beekeeper project.

project_name('Beekeeper').

:- initialization(load_bk).

load_bk:-
  source_file(load_bk, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),
  assert(user:file_search_path(bk, ThisDirectory)),
  assert(user:file_search_path(data, bk('Data'))),
  
  % Load the PGC.
  assert(user:file_search_path(pgc, bk('PGC'))),
  (
    predicate_property(debug_project, visible)
  ->
    ensure_loaded(pgc(debug))
  ;
    ensure_loaded(pgc(load))
  ),
  
  assert(user:file_search_path(lib, bk(lib))),
  
  use_module(bk(bk_web)).


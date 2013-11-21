% The run file for the Beekeeper project.

:- initialization(run_bk).

run_bk:-
  % Entry point.
  source_file(run_bk, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  
  % PGC
  load_pgc(project),
  
  % Beekeeper
  ensure_loaded(load).

load_pgc(_Project):-
  user:file_search_path(pgc, _Spec), !.
load_pgc(Project):-
  Spec =.. [Project,'PGC'],
  assert(user:file_search_path(pgc, Spec)),
  load_or_debug(pgc).

load_or_debug(Project):-
  predicate_property(debug_project, visible), !,
  Spec =.. [Project,debug],
  ensure_loaded(Spec).
load_or_debug(Project):-
  Spec =.. [Project,load],
  ensure_loaded(Spec).

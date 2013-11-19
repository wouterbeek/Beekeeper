% The load file for the Beekeeper project.

:- multifile(user:project_name/1).
user:project_name('Beekeeper').

:- initialization(load_bk).

load_bk:-
  % Entry point.
  source_file(load_bk, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  
  % Beekeeper
  assert(user:file_search_path(bk, ThisDir)),
  use_module(bk(bk_web)).


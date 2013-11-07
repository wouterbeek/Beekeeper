:- module(how, []).

/** <module> How does DataHives work?

@version 2013/11
*/

:- use_module(generics(meta_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).

:- web_module_add('How does DataHives work?', how, how).

% /img
:- db_add_novel(http:location(img, root(img), [])).
:- db_add_novel(user:file_search_path(img, bk(img))).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).

:- http_handler(root(how), how, []).

how(_Request):-
  reply_html_page(app_style, [], \dh_body).

dh_body -->
  html(
    body([
      h1('Waiting for the Semantic Web to happen...'),
      p(['The Semantic Web has been a great idea since 2001. ',
         'Unfortunately the use of Open and Linked Data is still limited to ',
         'a few (very) big companies and academics that research ',
         'the Semantic Web.'
      ]),
      \quote('Well, at least those researchers are eating their own dogfood...'),
      p(['The orginal idea of the Semantic Web was to create a universal ',
         'space of ideas.',
         'There are three reasons why this has not happended yet:'
      ]),
      img([align=right,src='img/triple_store.png']),
      ol([
        li([
          b('Infrastructural & financial barrier'),
          'Existing Semantic Web deployment systems require a server ',
          'that has continuous uptime.'
        ]),
        li([
          b('Management barrier'),
          'Once you have your triple store up and running, ',
          'it is not straightforward how to go about curating and enriching ',
          'your data.'
        ]),
        li([
          b('Sharing barrier'),
          'Once you have an interesting dataset it is not easy to share ',
          'your data with others. ',
          'Triples stores are often accessed via SPARQL endpoints ',
          'requiring the user to write complicate database-like queries.'
        ])
      ]),
      h1('So... how can DataHives help?'),
      p(['DataHives allows you to curate and share your Linked Open Data ']),
      ol([
        li('... without having to be online all of the time (just be online when you want to be)'),
        li('... without you having to set up a server.'),
        li('... without you having to write complicated queries that do not give the correct answer because of some syntactic qualm.')
      ]),
      \quote('Yeah, that\'s quite nice. But who wants to use this?'),
      h1('Use case 1: MSB and small governmental institutions'),
      p(['The Rijksmuseum has a Linked Data database of their collection. ',
         'This database was built in collaboration with Computer Scientists ',
         'over the past decade. ',
         'But not every museum is the Rijksmuseum...'
      ]),
      h1('Use case: individual consumer'),
      p(['A consumer may wants to share some data, ',
        'but is not generally able to run a Semantic Web server. ',
        'Currently a consumer can share his data through the services ',
        'provided by Internet companies such as Google and Facebook. ',
        'They publish their customer\'s  data as Linked Data. ',
        'However, the consumer looses ownership of the data ',
        'and is no longer able to manage his own data.'
      ]),
      p(['DataHives gives the ', i(consumer),
         ' control over his own data management process, ',
         'but without requiring him to be ....']),
      img(src='img/robert.png')
    ])
  ).

quote(Text) -->
  html(
    blockquote(
      style='font-family:\'Bookman Old Style\',Bookman,\'URW Bookman L\',serif;font-size:150%',
      [&(laquo), Text, &(raquo)]
    )
  ).

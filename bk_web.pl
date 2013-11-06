:- module(bk_web, []).

/** <module> Beekeeper Web

The Web interface for the beekeeper project.

@author Wouter Beek
@author Pepijn Kroes
@version 2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).

:- http_handler(root(bk), bk, []).

% /css
:- db_add_novel(http:location(css, root(css), [])).
:- db_add_novel(user:file_search_path(css, bk(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- html_resource(css('app.css'), []).

% /js
:- db_add_novel(http:location(js, root(js), [])).
:- db_add_novel(user:file_search_path(js, bk(js))).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).
:- html_resource(js('app.js'), []).
:- html_resource(js('behavior.js'), []).
:- html_resource(js('configuration.js'), []).
:- html_resource(js('connection.js'), []).
:- html_resource(js('d3.v3.min.js'), []).
:- html_resource(js('dataGenerator.js'), []).
:- html_resource(js('easy_setup.js'), []).
:- html_resource(js('n3parser.js'), []).
:- html_resource(js('peer.min.js'), []).
:- html_resource(js('rdfGraph.js'), []).
:- html_resource(js('swarm.js'), []).
:- html_resource(js('uicontroller.js'), []).
:- html_resource(js('utilities.js'), []).
:- html_resource(js('visualization.js'), []).
:- html_resource(js('zepto.min.js'), []).

:- web_module_add('Beekeeper', bk_web, bk).



algorithm_controls -->
  html(
    div(id=algorithmControls, [
      input([id=step,type=button,value=step]),
      input([id=animate,type=button,value=animate]),
      label([class=animationSpeed,for=speedSlider], 'speed: '),
      input([
        class=animationSpeed,
        id=speedSlider,
        max='2010',
        min='10',
        step='200',
        type=range
      ]),
      input([id=run,type=button,value=run])
      % <input type="checkbox" />
    ])
  ).

bk(_Request):-
  reply_html_page(app_style, \bk_head, \bk_body).

bk_body -->
  html(
    body(
      div(id=container, [
        \input,
        \controls,
        \graph,
        \status_message,
        \history,
        \load_screen
      ])
    )
  ).

bk_head -->
  html([
      title('Beekeeper'),
      %\html_requires(css('app.css')),
      \html_requires(js('app.js')),
      \html_requires(js('behavior.js')),
      \html_requires(js('configuration.js')),
      \html_requires(js('connection.js')),
      \html_requires(js('d3.v3.min.js')),
      \html_requires(js('dataGenerator.js')),
      \html_requires(js('easy_setup.js')),
      \html_requires(js('n3parser.js')),
      \html_requires(js('peer.min.js')),
      \html_requires(js('rdfGraph.js')),
      \html_requires(js('swarm.js')),
      \html_requires(js('uicontroller.js')),
      \html_requires(js('utilities.js')),
      \html_requires(js('visualization.js')),
      \html_requires(js('zepto.min.js'))
  ]).

controls -->
  html(div(id=controls, [\topbar,\sidebar])).

graph -->
  html(div(id=graph, [])).

history -->
  html(div(id=history, [])).

input -->
  html(
    div(id=input,
      textarea(id=inputN3, '
        <red wine> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <wine> .\c
        <white wine> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <wine> .\c
        <wine> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <beverage> .\c
        <beverage> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <food> .\c
        <food> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <consumable> .\c
        <liquid> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <beverage> .
      ')
    )
  ).

load_screen -->
  html(
    div(id=loadscreen,
      form(id=options, [
        p([
          input([id=hideOptions,type=button,value='▲']),
          label(for=userID, 'Name: '),
          input([id=userID,placeholder='User ID',type=text])
        ]),
        %<p>
        %  <input type="button" id="load" value="Paste RDF" />
        %  <input type="button" id="generate" value="Generate Graph" />
        %  <input type="button" id="saveFile" value="Save to File" />
        %</p>
        br([]),
        b('Experiment'),
        p([
          input([id=loadExperiment,type=button,value='Load Experiment']),
          input([id=saveExperiment,type=button,value='Save Experiment Data']),
          input([id=saveFile,type=button,value='Save Graph to File'])
        ]),
        b('Add a namespace to hosted datasets list'),
        p([
          %<input type="text" id="dataset" placeholder="dataset URI" />
          select([id=datasets,name=datasets], []),
          input([id=addDataset,type=button,value=add]),
          input([id=removeDataset,type=button,value=remove]),
          input([
            id=currentDatasets,
            type=button,
            value='current hosted datasets list'
          ])
        ]),
        b('On startup'),
        p([
          input([id=skipOptions,type=checkbox]),
          label(for=skipOptions, 'Skip this screen on startup')
        ]),
        p([
          input([id=connectOnLoad,type=checkbox]),
          label(for=connectOnLoad, 'Automatically connect to friends')
        ]),
        p([
          input([id=requestDatasetsOnLoad,type=checkbox]),
          label(
            for=requestDatasetsOnLoad,
            'Automatically request a hosted datasets list after connecting'
          )
        ]),
        b('On exit'),
        p([
          input([id=configlocalstorage,type=checkbox]),
          label(for=configlocalstorage,'Save configuration to local storage'),
          input([
            id=removeconfigfromlocalstorage,
            style='float: right;',
            type=button,
            value='clear from local storage'
          ])
        ]),
        p([
          input([disabled=disabled,id=graphlocalstorage,type=checkbox]),
          label(for=graphlocalstorage, 'Save graph(s) to local storage')
        ]),
        b('Algorithm'),
        p([
          input([id=linksetsEnabled,type=checkbox]),
          label(for=linksetsEnabled, 'Linksets enabled')
        ]),
        b('User Interface'),
        p([
          input([id=monitorEnabled,type=checkbox]),
          label(for=monitorEnabled, 'Show monitor')
        ]),
        b('Visualization'),
        p([
          input([id=visEnabled,type=checkbox]),
          label(for=visEnabled, 'Show visualization')
        ]),
        p([
          input([class=visOption,id=showLabels,type=checkbox]),
          label(for=showLabels, 'Show labels')
        ]),
        p([
          input([class=visOption,id=showMarkers,type=checkbox]),
          label(for=showMarkers, 'Show markers')
        ]),
        p([
          input([class=visOption,id=staticGraph,type=checkbox]),
          label(for=staticGraph, 'Static graph'),
          br([]),
          label(for=staticGraphIterations, 'Number of calculation cycles: '),
          input([
            class=visOption,
            id=staticGraphIterations,
            type=text,
            value='1000'
          ])
        ])
      ])
    )
  ).

sidebar -->
  html(
    div(id=sidebar, [
      input([id=showOptions,type=button,value='▼']),
      p(id=ownerID, []),
      input([id=connectID,placeholder='connect ID',type=text]),
      input([id=connect,type=button,value=connect]),
      div(id=friendsList, []),
      input([id=selectAllFriends,type=button,value='select all']),
      input([id=deselectAllFriends,type=button,value='deselect all']),
      br([]),
      br([]),
      %input([id=requestList,type=button,value='request nodes list']),
      input([
        id=requestHostedDatasets,
        type=button,
        value='request hosted datasets'
      ]),
      input([id=sendScouts,type=button,value='send scouts']),
      br([]),
      br([]),
      \algorithm_controls
    ])
  ).

status_message -->
  html(
    span(id='status-message', [
      span(id='sm-1', []),
      br([]),
      'Average cycle time: ',
      span(id='sm-2', '0'),
      br([]),
      'Scouts: ',
      span(id='sm-s', '0'),
      br([]),
      'Foragers: ',
      span(id='sm-f', '0'),
      br([]),
      'Nurse bees: ',
      span(id='sm-n', '0'),
      br([])
    ])
  ).

topbar -->
  html(
    div(id=topbar, [
      input([id=load,type=button,value='Paste RDF']),
      span(id=generateOptions, [
        input([id=generate,type=button,value='Generate Graph']),
        input([id=cancelGenerate,type=button,value='Cancel']),
        input([id=triplesToGenerate,type=text,value='50']),
        input([id=generateData,type=button,value='Randomly Generate Data']),
        input([id=namespaceToAdd,placeholder='Namespace URI',type=text]),
        input([id=addNamespace,type=button,value='Add namespace'])
      ]),
      input([id=files,onchange='loadFiles(this.files)',type=file]),
      % accept="text/plain, text/xml, application/xml, application/rdf+xml"
      % application/json
      br([]),
      % <input type="text" id="doData" />
      % <input type="button" id="do" value="do" />
      div(id=result)
    ])
  ).


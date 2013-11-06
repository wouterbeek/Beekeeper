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
:- use_module(server(app_server)).
:- use_module(server(web_modules)).

:- http_handler(root(bk), bk, [prefix,priority(100)]).

% /css
:- db_add_novel(http:location(css, root(css), [])).
:- db_add_novel(user:file_search_path(css, bk(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- html_resource('http://yui.yahooapis.com/pure/0.3.0/pure-min.css', []).
:- html_resource(
  'http://purecss.io/combo/1.6.5?/css/main.css&/css/menus.css&/css/rainbow/baby-blue.css',
  ['http://yui.yahooapis.com/pure/0.3.0/pure-min.css']
).
:- html_resource(
  css('app.css'), [
  'http://yui.yahooapis.com/pure/0.3.0/pure-min.css',
  'http://purecss.io/combo/1.6.5?/css/main.css&/css/menus.css&/css/rainbow/baby-blue.css'
]).

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

:- multifile(user:head//2).
:- multifile(user:body//2).



%! at_end// is det.
% We load the JavaScript files at the end, since
%   1. We do not know the interdependencies between the JavaScript files, and
%   2. We do not know the dependency of the JavaScript files on
%      the DOM being pre-loaded.
%
% This is the order given by PK:
% ~~~
% 1  <script src="lib/zepto.min.js"></script>
% 2  <script src="lib/n3parser.js"></script>
% 3  <script src="lib/d3.v3.min.js"></script>
% 4  <script src="lib/peer.min.js"></script>
% 5  <script src="configuration.js"></script>
% 6  <script src="behavior.js"></script>
% 7  <script src="dataGenerator.js"></script>
% 8  <script src="utilities.js"></script>
% 9  <script src="rdfGraph.js"></script>
% 10 <script src="visualization.js"></script>
% 11 <script src="swarm.js"></script>
% 12 <script src="connection.js"></script>
% 13 <script src="uicontroller.js"></script>
% 14 <script src="easy_setup.js"></script>
% 15 <script src="app.js"></script>
% ~~~

at_end -->
  {
    http_absolute_location(js('app.js'),           S15, []),
    http_absolute_location(js('behavior.js'),      S6,  []),
    http_absolute_location(js('configuration.js'), S5,  []),
    http_absolute_location(js('connection.js'),    S12, []),
    http_absolute_location(js('d3.v3.min.js'),     S3,  []),
    http_absolute_location(js('dataGenerator.js'), S7,  []),
    http_absolute_location(js('easy_setup.js'),    S14, []),
    http_absolute_location(js('n3parser.js'),      S2,  []),
    http_absolute_location(js('peer.min.js'),      S4,  []),
    http_absolute_location(js('rdfGraph.js'),      S9,  []),
    http_absolute_location(js('swarm.js'),         S11, []),
    http_absolute_location(js('uicontroller.js'),  S13, []),
    http_absolute_location(js('utilities.js'),     S8,  []),
    http_absolute_location(js('visualization.js'), S10, []),
    http_absolute_location(js('zepto.min.js'),     S1,  [])
  },
  html([
    script(src=S1,  []),
    script(src=S2,  []),
    script(src=S3,  []),
    script(src=S4,  []),
    script(src=S5,  []),
    script(src=S6,  []),
    script(src=S7,  []),
    script(src=S8,  []),
    script(src=S9,  []),
    script(src=S10, []),
    script(src=S11, []),
    script(src=S12, []),
    script(src=S13, []),
    script(src=S14, []),
    script(src=S15, [])
  ]).

bk(_Request):-
  reply_html_page(bk_style, [], []).

user:body(bk_style, _Content) -->
  html(
    body([
      div(class='pure-g-r', div(class='pure-u-1', \input)),
      div(class='pure-g-r', div(class='pure-u-1', \topbar)),
      div([class='pure-g-r',id=layout],[\left,\right]),
      \at_end
    ])
  ).

user:head(bk_style, _Content) -->
  html(
    head([
      title('Beekeeper (DataHives Event 1)'),
      \html_requires(css('app.css')),
      \html_requires('http://yui.yahooapis.com/pure/0.3.0/pure-min.css'),
      \html_requires('http://purecss.io/combo/1.6.5?/css/main.css&/css/menus.css&/css/rainbow/baby-blue.css')
      %\html_requires(js('app.js')),
      %\html_requires(js('behavior.js')),
      %\html_requires(js('configuration.js')),
      %\html_requires(js('connection.js')),
      %\html_requires(js('d3.v3.min.js')),
      %\html_requires(js('dataGenerator.js')),
      %\html_requires(js('easy_setup.js')),
      %\html_requires(js('n3parser.js')),
      %\html_requires(js('peer.min.js')),
      %\html_requires(js('rdfGraph.js')),
      %\html_requires(js('swarm.js')),
      %\html_requires(js('uicontroller.js')),
      %\html_requires(js('utilities.js')),
      %\html_requires(js('visualization.js')),
      %\html_requires(js('zepto.min.js'))
    ])
  ).

left -->
  html(
    div([class='pure-u-1-3',id=left],
      div([class=['grid-example','pure-g-r'],id=container], [
        div(class='pure-u-1-2', div(class='l-box', \sidebar)),
        div(class='pure-u-1-2', div(class='l-box', \history)),
        div(class='pure-u-1-2', div(class='l-box', \'status-message'))
      ])
    )
  ).

right -->
  html(
    div([class='pure-u-2-3',id=main],
      div(class=content, [\graph,\load_screen])
    )
  ).





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
    ])
  ).

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
      form([class=['pure-form','pure-form-aligned'],id=options], [
        input([id=hideOptions,type=button,value='▲']),
        fieldset(class='pure-group',
          input([class='pure-input-1-2',id=userID,placeholder='Username',type=text])
        ),
        %fieldset(class='pure-group', [
        %  button([class='pure-button',id=load], 'Paste RDF'),
        %  button([class='pure-button',id=generate], 'Generate Graph'),
        %  button([class='pure-button',id=saveFile], 'Save to File')
        %]),
        legend('Experiment'),
        fieldset(class='pure-group', [
          button([class='pure-button',id=loadExperiment],'Load Experiment'),
          button([class='pure-button',id=saveExperiment], 'Save Experiment Data'),
          button([class='pure-button',id=saveFile], 'Save Graph to File')
        ]),
        legend('Add a namespace to hosted datasets list'),
        fieldset(class='pure-group', [
          %input([class='pure-input-1-2',id=dataset,placeholder='Dataset URL',type=text]),
          select([class='pure-input-1-2',id=datasets,name=datasets]),
          button([class='pure-button',id=addDataset], 'Add'),
          button([class='pure-button',id=removeDataset], 'Remove'),
          button([class='pure-button',id=currentDatasets], 'Current hosted datasets list')
        ]),
        legend('On startup'),
        fieldset(class='pure-group', [
          label([class='pure-checkbox',for=skipOptions], [
            input([id=skipOptions,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Skip this screen on startup'
          ]),
          label([class='pure-checkbox',for=connectOnLoad], [
            input([id=connectOnLoad,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Automatically connect to friends'
          ]),
          label([class='pure-checkbox',for=requestDatasetsOnLoad], [
            input([id=requestDatasetsOnLoad,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
              'Automatically request a hosted datasets list after connecting'
          ])
        ]),
        legend('On exit'),
        fieldset(class='pure-group', [
          label([class='pure-checkbox',for=configLocalStorage], [
            input([id=configLocalStorage,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Save configuration to local storage'
          ]),
          button(
            [class='pure-button',id=removeConfigFromLocalStorage],
            'Clear from local storage'
          ),
          label([class='pure-checkbox',for=graphlocalstorage], [
            input([disabled=disabled,id=graphlocalstorage,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Save graph(s) to local storage'
          ])
        ]),
        legend('Algorithm'),
        fieldset(class='pure-group',
          label([class='pure-checkbox',for=linksetsEnabled], [
            input([id=linksetsEnabled,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Linksets enabled'
          ])
        ),
        legend('User Interface'),
        fieldset(class='pure-group',
          label([class='pure-checkbox',for=monitorEnabled], [
            input([id=monitorEnabled,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Show monitor'
          ])
        ),
        legend('Visualization'),
        fieldset(class='pure-group', [
          label([class='pure-checkbox',for=visEnabled], [
            input([id=visEnabled,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Show visualization'
          ]),
          label([class='pure-checkbox',for=showLabels], [
            input([class=visOption,id=showLabels,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Show labels'
          ]),
          label([class='pure-checkbox',for=showMarkers], [
            input([class=visOption,id=showMarkers,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Show markers'
          ]),
          label([class='pure-checkbox',for=staticGraph], [
            input([class=visOption,id=staticGraph,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Static graph'
          ])
        ]),
        legend('Cycles'),
        fieldset(class='pure-group', [
          label(for=staticGraphIterations, 'Number of calculation cycles: '),
          input([
            class=['pure-input-1-2',visOption],
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

'status-message' -->
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
        button([class='pure-button',id=generate], 'Generate Graph'),
        button([class='pure-button',id=cancelGenerate], 'Cancel'),
        input([class='pure-input-1-2',id=triplesToGenerate,placeholder='50',type=text]),
        button([class='pure-button',id=generateData], 'Randomly Generate Data'),
        input([class='pure-input-1-2',id=namespaceToAdd,placeholder='Namespace URI',type=text]),
        button([class='pure-button',id=addNamespace], 'Add namespace')
      ]),
      input([id=files,onchange='loadFiles(this.files)',type=file]),
      % accept="text/plain, text/xml, application/xml, application/rdf+xml"
      % application/json
      % <input type="text" id="doData" />
      % <input type="button" id="do" value="do" />
      div(id=result, [])
    ])
  ).

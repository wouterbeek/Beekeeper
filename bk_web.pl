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
:- use_module(library(http/http_session)).
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
  'http://purecss.io/combo/1.6.6?/css/main.css&/css/grids.css&/css/rainbow/baby-blue.css',
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
      div(class='pure-g-r',
        div([class='pure-u-1',id=pull], [
          div(class=['pure-menu','pure-menu-open','pure-menu-horizontal'],
            ul([
              li(id=pull_button1, button(class='pure-button', '▲ Options')),
              li(id=pull_button2, button(class='pure-button', '▲ Data')),
              li(id=pull_button3, button(class='pure-button', '▲ Logs')),
              li(id=pull_button4, button(class='pure-button', '▲ Connections'))
            ])
          ),
          \pull_content
        ])
      ),
      div(class='pure-g-r', div([class='pure-u-1',id=graph], [])),
      \at_end
    ])
  ).

user:head(bk_style, _Content) -->
  html(
    head([
      title('Beekeeper (DataHives Event 1)'),
      \html_requires('http://yui.yahooapis.com/pure/0.3.0/pure-min.css'),
      \html_requires('http://purecss.io/combo/1.6.5?/css/main.css&/css/menus.css&/css/rainbow/baby-blue.css'),
      \html_requires('http://purecss.io/combo/1.6.6?/css/main.css&/css/grids.css&/css/rainbow/baby-blue.css'),
      \html_requires(css('app.css'))
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



% PULL %

pull_content -->
  html(div(id=pull_content, [\options,\data,\logs,\connections])).



% DATA %

data -->
  {
    Content =
      '<red wine> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <wine> .\c
       <white wine> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <wine> .\c
       <wine> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <beverage> .\c
       <beverage> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <food> .\c
       <food> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <consumable> .\c
       <liquid> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <beverage> .'
  },
  html(
    div([class=some_pull_content,id=data], [
      form([class='pure-form',id=input], [
        fieldset(class='pure-group', [
          legend('Graph contents'),
          textarea([cols='60',id=inputN3,rows='4'], Content)
        ]),
        fieldset([class='pure-group',id=generateOptions], [
          legend('Auto generate'),
          button([class='pure-button',id=generate], 'Generate Graph'),
          input([style='display:inline-block;',id=triplesToGenerate,placeholder='50',type=text]),
          button([class='pure-button',id=generateData], 'Randomly Generate Data')
        ]),
        fieldset(class='pure-group', [
          legend('Namespaces'),
          input([style='display:inline-block;',id=namespaceToAdd,placeholder='Namespace URI',type=text]),
          button([class='pure-button',id=addNamespace], 'Add namespace')
        ]),
        div(class=[fileUpload,'pure-button','pure-button-primary'], [
          span('Upload'),
          input([class=upload,id=files,onchange='loadFiles(this.files)',type=file])
        ])
      ]),
      div(id=result, [])
    ])
  ).



% OPTIONS %

options -->
  html(
    div([class=some_pull_content,id=all_options],
      form(class=['pure-form','pure-form-aligned'], [
        fieldset(class='pure-group', [
          legend('Username'),
          label(id=userID, '')
        ]),
        fieldset(class='pure-group', [
          legend('Experiment'),
          button([class='pure-button',id=loadExperiment],'Load Experiment'),
          button([class='pure-button',id=saveExperiment], 'Save Experiment Data'),
          button([class='pure-button',id=saveFile], 'Save Graph to File')
        ]),
        fieldset(class='pure-group', [
          legend('Hosted namespaces'),
          select([style='display:inline-block;',id=namespaces], []),
          button([class='pure-button',id=addNamespace], 'Add'),
          button([class='pure-button',id=removeNamespace], 'Remove'),
          button([class='pure-button',id=currentNamespaces], 'Currently hosted namespaces')
        ]),
        fieldset(class='pure-group', [
          legend('On startup'),
          label([class='pure-checkbox',for=skipOptions], [
            input([id=skipOptions,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Skip this screen on startup'
          ]),
          label([class='pure-checkbox',for=connectOnLoad], [
            input([id=connectOnLoad,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Automatically connect to friends'
          ]),
          label([class='pure-checkbox',for=requestNamespacesOnLoad], [
            input([id=requestNamespacesOnLoad,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
              'Automatically request a hosted namespaces after connecting'
          ])
        ]),
        fieldset(class='pure-group', [
          legend('On exit'),
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
        fieldset(class='pure-group', [
          legend('Algorithm'),
          label([class='pure-checkbox',for=linksetsEnabled], [
            input([id=linksetsEnabled,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Linksets enabled'
          ])
        ]),
        legend('User Interface'),
        fieldset(class='pure-group',
          label([class='pure-checkbox',for=monitorEnabled], [
            input([id=monitorEnabled,style='display:inline;margin-right:7.5px;top:0;',type=checkbox]),
            'Show monitor'
          ])
        ),
        fieldset(class='pure-group', [
          legend('Visualization'),
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
        fieldset(class='pure-group', [
          legend('Cycles'),
          label(for=staticGraphIterations, 'Number of calculation cycles: '),
          input([
            class=visOption,
            id=staticGraphIterations,
            style='display:inline-block;',
            type=text,
            value='1000'
          ])
        ]),
        fieldset(class='pure-group',
          button([class=['pure-button','pure-button-primary'],id=confirm], 'Confirm')
        )
      ])
    )
  ).



% LOGS %

logs -->
  html(
    div([class=some_pull_content,id=logs], [
      form([class=['pure-form','pure-form-stacked'],id='status-message'],
        fieldset(class='pure-control-group', [
          legend('A Stacked Form'),
          label(id='sm-1', ''),
          label([for='sm-2'], 'Average cycle time: '),
          label(id='sm-2', '0'),
          label([for='sm-s'], 'Scouts: '),
          label(id='sm-s', '0'),
          label([for='sm-f'], 'Foragers: '),
          label(id='sm-f', '0'),
          label([for='sm-n'], 'Nurse bees: '),
          label(id='sm-n', '0')
        ])
      ),
      div(id=history, [])
    ])
  ).



% CONNECTIONS %

connections -->
  html(
    div([class=some_pull_content,id=connections],
      form(class='pure-form', [
        fieldset(class='pure-group', [
          legend('Owner'),
          label(id=ownerID, '')
        ]),
        fieldset(class='pure-group', [
          legend('Connect'),
          input([style='display:inline-block;',id=connectID,placeholder='Connect to user ID',type=text]),
          button([class='pure-button',id=connect], 'Connect')
        ]),
        fieldset(class='pure-group', [
          legend('Select'),
          div(id=friendsList, []),
          button([class='pure-button',id=selectAllFriends], 'Select all'),
          button([class='pure-button',id=deselectAllFriends], 'Deselect all')
        ]),
        fieldset(class='pure-group', [
          legend('Request'),
          button([class='pure-button',id=requestHostedNamespaces], 'Request hosted namespaces'),
          button([class='pure-button',id=sendScouts], 'Send scouts')
        ]),
        fieldset([class='pure-group',id=algorithmControls], [
          legend('Simulate'),
          button([class='pure-button',id=step], 'Step'),
          button([class='pure-button',id=animate], 'Animate'),
          button([class='pure-button',id=run], 'Run'),
          label([class=animationSpeed,for=speedSlider], 'speed: '),
          input([
            class=animationSpeed,
            id=speedSlider,
            max='2010',
            min='10',
            step='200',
            style='display: inline-block;',
            type=range
          ])
        ])
      ])
    )
  ).


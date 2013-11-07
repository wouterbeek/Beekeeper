var easy_setup = (function() {
  function start_setup() {
    config.easy_setup = true;
    
    hidePullover();
    
    config.skipLoadScreen = true;
    $('#skipOptions').get(0).checked = config.skipLoadScreen;
    
    config.requestNamespacesOnLoad = true;
    $('#requestNamespacesOnLoad').get(0).checked = config.requestNamespacesOnLoad;
    
    addToHostedNamespaces(config.ownerID);
    
    generateGraph(dataGenerator.generate(75));
    
    $('#animate').click();
    $('#speedSlider').val(2010);
    $('#speedSlider').mouseup();
  }
  
  return {
    start: start_setup
  };
})();

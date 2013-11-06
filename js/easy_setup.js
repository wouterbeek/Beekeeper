var easy_setup = (function() {
	
	function start_setup() {
		config.easy_setup = true;
		
		config.skipLoadScreen = true;
		$('#skipOptions').get(0).checked = config.skipLoadScreen;
		
		config.requestDatasetsOnLoad = true;
		$('#requestDatasetsOnLoad').get(0).checked = config.requestDatasetsOnLoad;
		
		$('#namespaceToAdd').val(config.ownerID);
		$('#addNamespace').click();
		
		$('#inputN3').val(dataGenerator.generate($('#triplesToGenerate').val()));
		generateGraph($('#inputN3').val());
		
		$('#animate').click();
		$('#speedSlider').val(2010);
		$('#speedSlider').mouseup();
		
		
	}
	
	return {
		
		start: start_setup
		
	};
	
})();
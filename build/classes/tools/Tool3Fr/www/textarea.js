$(document).ready(function(evt) {
	$tas = $(".inputTextarea");
    	$tas.each(function() { 
		$(this).text($.trim($(this).text())); 
	});
	
	$('.btn-file').html($('.btn-file').html().replace("Browse", "Parcourir"));
	$('.form-control[placeholder]').prop('placeholder', "Aucune sélection");
});
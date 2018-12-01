$(document).ready(function(evt) {
	$tas = $(".inputTextarea");
    	$tas.each(function() { 
		$(this).text($.trim($(this).text())); 
	});
	
	$('input[type=radio]').each(function() {
		var name = $(this).prop('name');
		if ( $(this).prop('id') == "" ) {
			$(this).prop('id', name.replace(".", "_") + "_" + $(this).val());
		}			
	});
	
	setTimeout(function() {
	  window.console.log("WTF");
	}, 4000);
});
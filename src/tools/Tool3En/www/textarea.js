$(document).ready(function(evt) {
	$tas = $(".inputTextarea");
    	$tas.each(function() { 
		$(this).text($.trim($(this).text())); 
	});
	
	setTimeout(function() {
	  window.console.log("WTF");
	}, 5000);
});
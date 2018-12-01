$(document).ready(function(evt) {
	$tas = $(".inputTextarea");
    	$tas.each(function() { 
		$(this).text($.trim($(this).text())).css('color', 'black'); 
	});
	
	$('li.treeview').find('a:first').click(function(e) { e.preventDefault(); return false; });
});
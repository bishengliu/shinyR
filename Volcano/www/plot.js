$('#myModal').on('show.bs.modal', function(e) {
	$('input#SelectGeneId').val(e.relatedTarget.id);
	$('input#NewGeneId').val(e.relatedTarget.id);
	$('#GeneId').text('Current Gene ID: ' + e.relatedTarget.id);
	});
/*
$('#myModal').on('show.bs.modal', function(e){
		
		var o = new Option(e.relatedTarget.id, e.relatedTarget.id);
		$(o).html(e.relatedTarget.id);
		$("#NewGeneId").append(o);
		
		
		$('#NewGeneId').append($('<option>', {
			value: e.relatedTarget.id,
			text: e.relatedTarget.id,
		}));

		$("#NewGeneId option[value=" + e.relatedTarget.id+"]").attr('selected','selected');
	})
	
*/

$("text[text-archor='start']").hide()
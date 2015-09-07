$('#myModal').on('show.bs.modal', function(e) {
	$('input#SelectGeneId').val(e.relatedTarget.id);
	$('#GeneId').text('Gene ID: ' + e.relatedTarget.id);
	});

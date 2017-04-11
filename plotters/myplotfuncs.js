function updateplot() {
    
    var fileInput = document.getElementById('fileInput');
    var errorout = document.getElementById('error out');    
    
    var file = fileInput.files[0];
    var textType = /text.*/;

    if (file.type.match(textType)) {
	var reader = new FileReader();
	reader.readAsText(file);	
	reader.onload = function(e) {
	    
	    lineplot('container',
		     document.getElementById('pltitle').value,
		     document.getElementById('plsubtitle').value,
		     document.getElementById('plxlabel').value,
		     document.getElementById('plylabel').value,
		     reader.result);
	}
	
    } else {
	errorout.innerText = "File not supported!"
    }
    
}

function lineplot(container, titletxt, subtxt, xaxistxt, yaxistxt, datacsv) {

    Highcharts.chart(container, {
	title: { text: titletxt },
	subtitle: { text: subtxt },
	yAxis: { title: { text: yaxistxt } },
	xAxis: { title: { text: xaxistxt } },	
	legend: { layout: 'vertical', align: 'right', verticalAlign: 'middle' },
	data: {	csv: datacsv }
//	plotOptions: { series: { pointStart: 2010 } },
    });

}

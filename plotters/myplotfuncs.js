function updateplot() {
    
    var fileInput = document.getElementById('fileInput');
    var errorout = document.getElementById('error out');
    var plottype;


    if(document.getElementById('plottype1').checked) {
	plottype = document.getElementById('plottype1').value;
    } else if(document.getElementById('plottype2').checked) {
	plottype = document.getElementById('plottype2').value;
    } else if(document.getElementById('plottype3').checked) {
	plottype = document.getElementById('plottype3').value;
    } else {
	plottype = document.getElementById('plottype4').value;
    }
    
    var file = fileInput.files[0];
    var textType = /text.*/;

    if (file.type.match(textType)) {
	var reader = new FileReader();
	reader.readAsText(file);	
	reader.onload = function(e) {
	    
	    makeplot('container',
		     plottype,
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

function makeplot(container, plottype, titletxt, subtxt, xaxistxt, yaxistxt, datacsv) {

    Highcharts.chart(container, {
	chart: { type: plottype },
	title: { text: titletxt },
	subtitle: { text: subtxt },
	yAxis: { title: { text: yaxistxt } }, 
	xAxis: { title: { text: xaxistxt } },
	legend: { enable: 'true' , layout: 'vertical', align: 'right', verticalAlign: 'middle' },
	data: { csv: datacsv },
/*	tooltip: {
	    formatter: function() {
		var sliceIndex = this.point.index;
		var sliceName = this.series.chart.axes[0].categories[sliceIndex];
		return 'The value for <b>' + sliceName +
		    '</b> is <b>' + this.y + '</b>';
	    }
	}	*/
//	plotOptions: { series: { pointStart: 2010 } },
    });

}

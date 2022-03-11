function strip_comma(str){
	if(str === undefined){
		return '';
	}
	return str.replace(/,/g, '');
}

function make_line(dt){
	var line = '';
	line = line.concat(strip_comma(dt['Adresa'])).concat(',');
	line = line.concat(strip_comma(dt['Cod Judet Locatie'])).concat(',');
	line = line.concat(strip_comma(dt['Cod SIRUTA'])).concat(',');
	line = line.concat(strip_comma(dt['Denumire/Identificare locatie'])).concat(',');
	line = line.concat(strip_comma(dt['Exista rampa acces'])).concat(',');
	line = line.concat(strip_comma(dt['ID Locatie'])).concat(',');
	line = line.concat(strip_comma(dt['ID Internet'])).concat(',');
	line = line.concat(strip_comma(dt['Latitudine'])).concat(',');
	line = line.concat(strip_comma(dt['Localitate UAT'])).concat(',');
	line = line.concat(strip_comma(dt['Longitudine'])).concat(',');
	line = line.concat(strip_comma(dt['Nr. centre/locatie']));
	return line;
}

var timestamp = new Date().getTime();

$.ajax({
    url: 'https://vaccinare-covid.gov.ro/wp-content/themes/twentytwenty/assets/map/csv/centre_vaccinare_etapa_3.php?v='+timestamp,
    type: "GET",
    dataType: "json",
    success: function (data) {

    	var data_csv = 'address,county,siruta_code,name,access_ramp,location_id,internet_id,latitude,uat,longitude,center_count\n';

    	for(i=0;i<data.length;i++){
    		if( data[i]['Latitudine'] != null && data[i]['Latitudine'] != "" ){
    			l = make_line(data[i])
    			data_csv = data_csv.concat(l).concat('\n')
    		}
    	}

    	var a = document.createElement("a");
    	var file = new Blob([data_csv], {type: 'text/plain'});
  		var url = URL.createObjectURL(file);
        a.href = url;
        a.download = "centre_covid.csv";
        document.body.appendChild(a);
        a.click();
    },
    error: function (error) {
        console.log(`Error ${error}`);
    }
});

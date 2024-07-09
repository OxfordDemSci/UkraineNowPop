import * as _init from './init.js?version=0.19'

export function get_migration_probabilities2(api_url, country, admin_level = 1, date, admin_id=null, age_min_mal, age_max_male, age_min_female, age_max_female, accessToken="null"){
    
    if (api_url.substr(-1) !== '/')
        api_url += '/';
    
    const fullUrl = api_url + 'get_migration_probabilities';
    
    let age_max_female_f = age_max_female >= 85 ? 80 : age_max_female;
    let age_max_male_f = age_max_male >= 85 ? 80 : age_max_male;

    let data = {};
    data = {
        country: country,
        admin_level: admin_level,
        date: date,
        admin_id: admin_id,
        age_min_mal: age_min_mal,
        age_max_male: age_max_male_f,
        age_min_female: age_min_female,
        age_max_female: age_max_female_f,
        rank_by: "probability",
        limit: $('#numberMigrationLimit').children("option:selected").val()
    };
    
    var result = "";
    $.ajax({
        url: fullUrl,
        async: false,
        type: 'get',
        data: data,
        dataType: 'json',
        beforeSend: function (request) {
            //console.log(request.url + "?" + request.data);
            if (accessToken !== "null"){
                request.setRequestHeader("Authorization", accessToken);
            }
        },        
        success: function (data) {
            result = data;
        },
        error: function (xhr, ajaxOptions, thrownError) {
            console.log(xhr.status);
            console.log(thrownError);
        }
    });
    return result;
    
}


export function update_total_in_out_for_admin(pcdoe, d) {
    
    let total_in = 0; 
    let total_out = 0; 
   

    var values_flows = [];

   for (var key of Object.keys(d)) {
        for (var i = 0; i < d[key].length; i++) {
            if (key === pcdoe){
                total_out = total_out + d[key][i]["count"];
            }else{
                if (d[key][i].destination === pcdoe){
                total_in = total_in +  d[key][i].count;  
                }
            }
        }           
   }

document.getElementById('adminMigrationInLabel').innerHTML = total_in;
document.getElementById('adminMigrationOutLabel').innerHTML = total_out;
document.getElementById('adminMigrationTotalLabel').innerHTML = total_in - total_out;


}

export function update_migration_probabilities(d, series) {
    
    let typeData="count";
    
    if(document.getElementById('btnradioPlotChordDiagramCount').checked === true) {   
         typeData="count";   
        } else {  
         typeData="probability";   
    }      

    var values_flows = [];

    for (var key of Object.keys(d)) {

        for (var i = 0; i < d[key].length; i++) {
            //if (d[key][i].probability > 10.3) {
                values_flows.push(
                        {
                            from: key,
                            to: d[key][i].destination,
                            value: d[key][i][typeData]
                        }
                );
            //}
        }

    }

    series.data.setAll(values_flows);
    series.appear(100, 10);


}



export function update_migration_probabilities_LG(d, series, root) {

//
//    let series_plotChordDiagramt_LG = null;
//    series_plotChordDiagramt_LG = _init.initialise_migration_probabilities_chart_LG(root_plotChordDiagramt_LG);


    let typeData = "count";

    if (document.getElementById('btnradioPlotChordDiagramCount').checked === true) {
        typeData = "count";
    } else {
        typeData = "probability";
    }

    var values_flows = [];

    for (var key of Object.keys(d)) {

        for (var i = 0; i < d[key].length; i++) {

            values_flows.push(
                    {
                        from: key,
                        to: d[key][i].destination,
                        value: d[key][i][typeData]
                    }
            );
        }

    }


    series.data.setAll(values_flows);
    

//    series.bullets.push(function (_root, _series, dataItem) {
//        var bullet = am5.Bullet.new(root, {
//            locationY: Math.random(),
//            sprite: am5.Circle.new(root, {
//                radius: 5,
//                fill: dataItem.get("source").get("fill")
//            })
//        });
//
//        bullet.animate({
//            key: "locationY",
//            to: 1,
//            from: 0,
//            duration: Math.random() * 1000 + 2000,
//            loops: Infinity
//        });
//
//        return bullet;
//    });

    //event.preventDefault();
// https://www.amcharts.com/docs/v5/concepts/common-elements/bullets/
    //series.bullets.clear();

}
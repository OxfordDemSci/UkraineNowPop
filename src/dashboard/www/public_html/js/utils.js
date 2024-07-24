export function get_admin_units_geo2(api_url, country , admin_level = 1, accessToken="null") {
    // headers: {"Authorization": localStorage.getItem('token')}
    //POSTGRES_USER=oxford_now_pop_admin
    //POSTGRES_PASSWORD=~ZGs+B858m0J
    // headers: {"Authorization": null},

    //let accessToken  = "Bearer " + "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmcmVzaCI6ZmFsc2UsImlhdCI6MTcwOTc3MzQ5NSwianRpIjoiMTFmYjRlYTctMWRjZC00MjgxLWI3NDEtMjYyMjlhMzhlYTcwIiwidHlwZSI6ImFjY2VzcyIsInN1YiI6Im94Zm9yZF9ub3dfcG9wX2FkbWluIiwibmJmIjoxNzA5NzczNDk1LCJjc3JmIjoiZWNiNzBlM2ItMTk2Yy00OWE5LTk5MjQtZGFiZDdmNzlhZGVjIiwiZXhwIjoxNzA5NzgwNjk1LCJzY29wZSI6IndyaXRlIn0.6cnqyOeyRP7YHcq1Vf9nqGmnAlz0QlsHyTPmc6p69uk";
    //accessToken="null";
    if (api_url.substr(-1) !== '/')
        api_url += '/';
    const fullUrl = api_url + 'get_admin_units';

    let data = {};
    data = {
        country: country,
        admin_level:admin_level
    };
    
    var result = "";
    $.ajax({
        url: fullUrl,
        async: false,
        type: 'get',
        data: data,
        dataType: 'json',
        beforeSend: function (request) {
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

export function get_admin_units_geo(api_url, country , admin_level = 1, accessToken="null") {
    var result = "";
    $.ajax({
        url: './data/admin_UKR_level_'+admin_level+'.geojson',
        async: false,
        type: 'get',
        dataType: 'json',
        success: function (data) {
            result = data;
        },
        error: function (xhr, ajaxOptions, thrownError) {
            console.log(xhr.status);
            console.log(thrownError);
        },
        complete: function () {
            //_utils.progressMenuOff();
        }
    });
    return result;
}


export function get_population2(api_url, country , admin_level = 1, date, admin_id=null, age_min_mal, age_max_male, age_min_female, age_max_female, accessToken="null") {
    
    if (api_url.substr(-1) !== '/')
        api_url += '/';
    
    const fullUrl = api_url + 'get_population';
    
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
        age_max_female: age_max_female_f
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

export function get_population_promis2(api_url, 
                                      country, 
                                      admin_level = 1, 
                                      date, 
                                      admin_id=null, 
                                      age_min_mal, 
                                      age_max_male, 
                                      age_min_female, 
                                      age_max_female, 
                                      accessToken="null") {
    
    if (api_url.substr(-1) !== '/')
        api_url += '/';
    
    const fullUrl = api_url + 'get_population';
    
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
        age_max_female: age_max_female_f
    };
    
//    var result = "";
//    $.ajax({
//        url: fullUrl,
//        async: false,
//        type: 'get',
//        data: data,
//        dataType: 'json',
//        beforeSend: function (request) {
//            //console.log(request.url + "?" + request.data);
//            if (accessToken !== "null"){
//                request.setRequestHeader("Authorization", accessToken);
//            }
//        },        
//        success: function (data) {
//            result = data;
//        },
//        error: function (xhr, ajaxOptions, thrownError) {
//            console.log(xhr.status);
//            console.log(thrownError);
//        }
//    });
//    return result;
    
    
    
    return new Promise((resolve, reject) => {
        $.ajax({
            url: fullUrl,
            async: true,
            type: 'get',
            data: data,
            dataType: 'json',
            beforeSend: function (request) {
                if (accessToken !== "null"){
                    request.setRequestHeader("Authorization", accessToken);
                }
                //console.log(url);
            },
            success: function (data) {
                resolve(data);
            },
            error: function (error) {
                reject(error);
            }
        });
    });
}


export function get_admin_names(d) {
    return d.admin_names[0];
}

export function setGeoMenu(t) {

    const list = document.getElementById('idSelectGeoLevel');
    
    let k;
    
    list.innerHTML = "";
    for (var i = 1; i < Object.keys(t).length+1; i++) {

        k = Object.keys(t)[i-1];
        
        if ((localStorage.getItem('access')) === "true" || k === "adm1_name") {
            list.innerHTML = list.innerHTML +
                    '<option value="' + i + '" >' + t[k] + '</option>';
        } else {
            list.innerHTML = list.innerHTML +
                    '<option value="' + i + '" disabled>' + t[k] + '</option>';
        }
    }
}

export function getAgeRanges(d) {

//    var age_ranges = [];
//    age_ranges.push(d.age_ranges[0]["age_min"]);
//    for (var i = 1; i < d.age_ranges.length; i++) {
//        age_ranges.push(d.age_ranges[i]["age_min"]);
//    }
//    
//    return age_ranges;
    
    
    var age_ranges_min = [];
    var age_ranges_max = [];
    age_ranges_min.push(d.age_ranges[0]["age_min"]);
    age_ranges_max.push(d.age_ranges[0]["age_max"]);
    for (var i = 1; i < d.age_ranges.length; i++) {
        age_ranges_min.push(d.age_ranges[i]["age_min"]);
        age_ranges_max.push(d.age_ranges[i]["age_max"]);
    }
    
    return [age_ranges_min,age_ranges_max];
    
}

export function getDates (d) {

    var dates_available = [];
    for (var i = 0; i < d.length; i++) {
        dates_available.push(d[i]);
    }
    return dates_available;
}

export function progressMenuOn() {
    document.getElementById("firstloader").style.visibility = 'visible';
    document.getElementById("firstloader").style.display = 'block';
}

export function progressMenuOff() {
    document.getElementById("firstloader").style.visibility = 'hidden';
    document.getElementById("firstloader").style.display = 'none';
}


export function nFormatter(num, digits) {
  const lookup = [
    { value: 1, symbol: "" },
    { value: 1e3, symbol: "k" },
    { value: 1e6, symbol: "M" },
    { value: 1e9, symbol: "G" },
    { value: 1e12, symbol: "T" },
    { value: 1e15, symbol: "P" },
    { value: 1e18, symbol: "E" }
  ];
  const regexp = /\.0+$|(?<=\.[0-9]*[1-9])0+$/;
  const item = lookup.findLast(item => num >= item.value);
  //return item ? (num / item.value).toFixed(digits).replace(regexp, "").concat(item.symbol) : "0";
  return  item ? (num / item.value).toFixed(digits).concat(item.symbol) : "0";
}

export function nFormatter_Space(num, digits) {
  const lookup = [
    { value: 1, symbol: "" },
    { value: 1e3, symbol: " k" },
    { value: 1e6, symbol: " M" },
    { value: 1e9, symbol: " G" },
    { value: 1e12, symbol: " T" },
    { value: 1e15, symbol: " P" },
    { value: 1e18, symbol: " E" }
  ];
  const regexp = /\.0+$|(?<=\.[0-9]*[1-9])0+$/;
  const item = lookup.findLast(item => num >= item.value);
  //return item ? (num / item.value).toFixed(digits).replace(regexp, "").concat(item.symbol) : "0";
  return  item ? (num / item.value).toFixed(digits).concat(item.symbol) : "0";
}

export function sumNumbersInJSON(obj) { 
  let total = 0; 
 
  function addNumbers(obj) { 
    for (let key in obj) { 
      if (typeof obj[key] === 'number') { 
        total += obj[key]; 
      } else if (typeof obj[key] === 'object') { 
        addNumbers(obj[key]); // recursively call addNumbers for nested objects 
      } 
    } 
  } 
 
  addNumbers(obj); // start the recursion with the top-level object 
  return total; 
}

export function sumFemaleInJSON(obj) { 
  let total = 0; 
    for (var key of Object.keys(obj)) {
                total = total + obj[key].female_population; 
    }
 return total; 
}

export function sumMaleInJSON(obj) { 
  let total = 0; 
    for (var key of Object.keys(obj)) {
                total = total + obj[key].male_population; 
    }
  return total; 
}

export function sumMaleFemaleInAdmin(obj) { 
  let total = 0; 
    for (var key of Object.keys(obj)) {
                total = total + obj[key].population; 
    }
  return total; 
}

export function round5(x)
{
    return Math.ceil(x / 5) * 5;
}


export function resetSlider_age_selections(d)
{
    
        $(".slider_age_selections").slider({
            min: 0,
            max: d[0].length - 1,
            range: true,
            values: [0, d[0].length - 1]
        })
//        .slider("float", {
//            labels: d
//        })
        .slider("pips", {
            labels: {first: "0", last: d[0][d[0].length - 1] + "+"}
        });
        
        let age_min = 0;    
        let age_max = d[0][d[0].length - 1];
        document.getElementById('label_Age_range').innerHTML = "Ages: "+ age_min +" - "+ age_max;
        
        event.preventDefault();
}

export function resetDate_slider(d, dates_devided_label)
{
        $(".Date-slider").slider({
            min: 0,
            max: d.length - 1,
            value: d.length - 1
        })  .slider("pips", "refresh");
//        .slider("float", {
//            labels: d
//        })
//        .slider("pips", {
//            rest: "label",
//            labels: d,
//            step: Math.ceil(d.length/dates_devided_label)
//        });
        event.preventDefault();
}


export function isObjectEmpty(objectName)
{
    for (let prop in objectName) {
        if (objectName.hasOwnProperty(prop)) {
        return false;
        }
    }
    return true;
}


export function update_Age_Range_labele(vFirst, vLast , age_ranges_available)
{
    let age_min = age_ranges_available[0][vFirst];    
    let age_max = age_ranges_available[1][vLast];
    if (vLast === age_ranges_available[1].length-1){
          age_max = age_ranges_available[0][age_ranges_available[0].length - 1] + "+";
    }    
    document.getElementById('label_Age_range').innerHTML = "Ages: "+ age_min +" - "+ age_max;
}


/*
a - let txt_Geo_DropDown 
b - txt_Sex_DropDown 
c - txt_Country_Total_Title 
d - txt_Date_Bottom_Panel  
 */
export function update_Panels_labels(a, b, c, d, _dates_available)
{
    document.getElementById('lb_Geo_DropDown').innerHTML = a;   
    document.getElementById('lb_Sex_DropDown').innerHTML = b;   
    document.getElementById('lb_Country_Total_Title').innerHTML = c + " [ " + _dates_available[_dates_available.length-1] + " ] ";   
    document.getElementById('lb_Date_DateTimePanel').innerHTML = d;   
}


export function update_mainPanels_Totalslabels(result)
{
    let SelectedSex = $('#idSelectSex option').filter(":selected").val();

    if (SelectedSex === "Female") {
        document.getElementById('cntrlTotalLabel').innerHTML = "-";
        document.getElementById('cntrlTotalFemalesLabel').innerHTML = sumFemaleInJSON(result.population_totals_by_sex).toLocaleString().replace(/,/g," ",);
        document.getElementById('cntrlTotalMalesLabel').innerHTML = "-";
    }
    if (SelectedSex === "Male") {
        document.getElementById('cntrlTotalLabel').innerHTML = "-";
        document.getElementById('cntrlTotalFemalesLabel').innerHTML = "-";
        document.getElementById('cntrlTotalMalesLabel').innerHTML = sumMaleInJSON(result.population_totals_by_sex).toLocaleString().replace(/,/g," ",);
    }
    if (SelectedSex === "Both") {
        document.getElementById('cntrlTotalLabel').innerHTML = sumNumbersInJSON(result.population_totals).toLocaleString().replace(/,/g," ",);
        document.getElementById('cntrlTotalFemalesLabel').innerHTML = sumFemaleInJSON(result.population_totals_by_sex).toLocaleString().replace(/,/g," ",);
        document.getElementById('cntrlTotalMalesLabel').innerHTML = sumMaleInJSON(result.population_totals_by_sex).toLocaleString().replace(/,/g," ",);
    }    

}

export function update_mainPanels_AdminTotalslabels(result_pyramid)
{
    let SelectedSex = $('#idSelectSex option').filter(":selected").val();

    if (SelectedSex === "Female") {
        document.getElementById('adminFemaleTotalLabel').innerHTML = sumMaleFemaleInAdmin(result_pyramid.female_population).toLocaleString().replace(/,/g," ",);
        document.getElementById('adminMaleTotalLabel').innerHTML = "-";
        document.getElementById('adminTotalLabel').innerHTML = "-";
    }
    if (SelectedSex === "Male") {
        document.getElementById('adminFemaleTotalLabel').innerHTML = "-";
        document.getElementById('adminTotalLabel').innerHTML = "-";
        document.getElementById('adminMaleTotalLabel').innerHTML = sumMaleFemaleInAdmin(result_pyramid.male_population).toLocaleString().replace(/,/g," ",);
    }
    if (SelectedSex === "Both") {
        let tM = sumMaleFemaleInAdmin(result_pyramid.male_population);
        let tF = sumMaleFemaleInAdmin(result_pyramid.female_population);
        let tMF= tM + tF;
        document.getElementById('adminMaleTotalLabel').innerHTML = tM.toLocaleString().replace(/,/g," ",);
        document.getElementById('adminFemaleTotalLabel').innerHTML = tF.toLocaleString().replace(/,/g," ",);
        document.getElementById('adminTotalLabel').innerHTML = tMF.toLocaleString().replace(/,/g," ",);
    }    

}


export function preproces_results_for_updatePopulationMap(result)
{
    var result_out = [];
    
    let SelectedSex = $('#idSelectSex option').filter(":selected").val();

    if (SelectedSex === "Female") {
            for (let key in result.population_totals_by_sex) { 
                result_out[key]=  result.population_totals_by_sex[key].female_population;                     
            }
    }
    if (SelectedSex === "Male") {
            for (let key in result.population_totals_by_sex) { 
                result_out[key]=  result.population_totals_by_sex[key].male_population;                     
            }
    }
    if (SelectedSex === "Both") {
            for (let key in result.population_totals_by_sex) { 
                result_out[key]=  result.population_totals_by_sex[key].male_population + result.population_totals_by_sex[key].female_population;                     
            }
    }
    
    return result_out;

}

export function parsing_string_date_new_format(d)
{
    var odate = new Date(d);
    let ndate= odate.toISOString().replace(/^(\d+)-(\d+)-(\d+)T(\d+):(\d+):(\d+).(\d+)Z$/, function (a,y,m,d) {return [d,['Jan','Feb','Mar','Apr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic'][m-1],y].join('-');});
    return ndate;
}

export function get_adminunits_names(geoJson)
{
    var adminunits_names = [];

    for (var i = 0; i < geoJson.features.length; i++) {
            var pcode = geoJson.features[i].properties.pcode;
            var name_en = geoJson.features[i].properties.name_en;
               adminunits_names.push(
                {pcode: pcode, name: name_en}
               );
            
    } 
    
    return adminunits_names;
}

//function formatDate(date) {
//    date.toISOString()
//    .replace(/^(\d+)-(\d+)-(\d+).*$/, // Only extract Y-M-D
//        function (a,y,m,d) {
//            return [
//                d, // Day
//                ['Jan','Feb','Mar','Apr','May','Jun',  // Month Names
//                'Jul','Ago','Sep','Oct','Nov','Dec']
//                [m-1], // Month
//                y  // Year
//            ].join('-'); // Stitch together
//        });
//}
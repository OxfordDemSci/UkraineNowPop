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

    var age_ranges = [];
    age_ranges.push(d.age_ranges[0]["age_min"]);
    for (var i = 1; i < d.age_ranges.length; i++) {
        age_ranges.push(d.age_ranges[i]["age_min"]);
    }
    
    return age_ranges;
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
            max: d.length - 1,
            range: true,
            values: [0, d.length - 1]
        })
        .slider("float", {
            labels: d
        })
        .slider("pips", {
            labels: {first: d[0].toString(), last: d[d.length - 1] + "+"}
        });
        event.preventDefault();
}

export function resetDate_slider(d)
{
        $(".Date-slider").slider({
            min: 0,
            max: d.length - 1,
            value: d.length - 1
        })
        .slider("float", {
            labels: d
        })
        .slider("pips", {
            rest: "label",
            labels: d,
            step: 10
        });
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
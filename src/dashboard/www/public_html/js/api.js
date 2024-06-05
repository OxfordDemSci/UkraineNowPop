import * as _utils from './utils.js?version=0.46'

export function get_population(api_url, 
                               country, 
                               admin_level, 
                               admin_id, 
                               dates_available, 
                               age_ranges_available,
                               accessToken) {
    
    let date = dates_available[$(".Date-slider").slider("value")];       

    let age_min_male =  _utils.round5(age_ranges_available[$(".slider_age_selections").slider("values", 0)]);
    let age_max_male =  age_ranges_available[$(".slider_age_selections").slider("values", 1)];
    let age_min_female =  _utils.round5(age_ranges_available[$(".slider_age_selections").slider("values", 0)]);
    let age_max_female =  age_ranges_available[$(".slider_age_selections").slider("values", 1)];
    
    if (api_url.substr(-1) !== '/')
        api_url += '/';
    
    const fullUrl = api_url + 'get_population';
    
    let age_max_female_f = age_max_female >= 85 ? 80 : age_max_female;
    let age_max_male_f = age_max_male >= 85 ? 80 : age_max_male;
    
    let SelectedSex=$('#idSelectSex option').filter(":selected").val();
 
    if (SelectedSex === "Female"){
           age_min_male =  0;
           age_max_male_f =  0;
    }
    if (SelectedSex === "Male"){
           age_min_female =  0;
           age_max_female_f =  0;
    }    

    let data = {};
    data = {
        country: country,
        admin_level: admin_level,
        date: date,
        admin_id: admin_id,
        age_min_male: age_min_male,
        age_max_male: age_max_male_f,
        age_min_female: age_min_female,
        age_max_female: age_max_female_f
    };
   
    return new Promise((resolve, reject) => {
        $.ajax({
            url: fullUrl,
            async: true,
            type: 'get',
            data: data,
            dataType: 'json',
            beforeSend: function (request) {
                if (localStorage.getItem('access') === "true" ){
                    request.setRequestHeader("Authorization", "Bearer " + accessToken);
                }
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



export function get_migration_probabilities(api_url, 
                                            country, 
                                            admin_level, 
                                            admin_id, 
                                            dates_available, 
                                            age_ranges_available,
                                            accessToken){
  
  
    let rank_by="count";
    
    if(document.getElementById('btnradioPlotChordDiagramCount').checked === true) {   
         rank_by="count";   
        } else {  
         rank_by="probability";   
    }  
    
    let date = dates_available[$(".Date-slider").slider("value")];   
    
    let age_min_male =  _utils.round5(age_ranges_available[$(".slider_age_selections").slider("values", 0)]);
    let age_max_male =  age_ranges_available[$(".slider_age_selections").slider("values", 1)];
    let age_min_female =  _utils.round5(age_ranges_available[$(".slider_age_selections").slider("values", 0)]);
    let age_max_female =  age_ranges_available[$(".slider_age_selections").slider("values", 1)];

    if (api_url.substr(-1) !== '/')
        api_url += '/';
    
    const fullUrl = api_url + 'get_migration_probabilities';
    
    let age_max_female_f = age_max_female >= 85 ? 80 : age_max_female;
    let age_max_male_f = age_max_male >= 85 ? 80 : age_max_male;
    
    let SelectedSex=$('#idSelectSex option').filter(":selected").val();
 
    if (SelectedSex === "Female"){
           age_min_male =  0;
           age_max_male_f =  0;
    }
    if (SelectedSex === "Male"){
           age_min_female =  0;
           age_max_female_f =  0;
    }      

    let data = {};
    data = {
        country: country,
        admin_level: admin_level,
        date: date,
        admin_id: admin_id,
        age_min_male: age_min_male,
        age_max_male: age_max_male_f,
        age_min_female: age_min_female,
        age_max_female: age_max_female_f,
        rank_by: rank_by,
        limit: $('#numberMigrationLimit').children("option:selected").val()
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
                if (localStorage.getItem('access') === "true" ){
                    request.setRequestHeader("Authorization", "Bearer " + accessToken);
                }
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


export function get_sign_in(api_url, username, password){
  

    if (api_url.substr(-1) !== '/')
        api_url += '/';
    
    const fullUrl = api_url + 'login';

    let result = {};

    $.ajax({
        url: fullUrl,
        async: false,
        type: 'POST',
        data: JSON.stringify({
            username: username,
            password: password
        }),
        dataType: 'json',
        headers: { 
            'Accept': 'application/json',
            'Content-Type': 'application/json' 
        },        
        success: function (data) {
            result = {
                status: 200,
                access_token: data.access_token
            };
        },
        error: function (xhr, ajaxOptions, thrownError) {
            console.log(xhr.status);
            console.log(thrownError);
            $('#passwordsNoMatchRegister').show();
            document.getElementById('passwordsNoMatchRegisterLabel').innerHTML =  thrownError; 
            result = {
                status: xhr.status,
                access_token:""
            };            
        }
    });
    return result;
    //event.preventDefault();
}


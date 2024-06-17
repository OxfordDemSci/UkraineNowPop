var API_URL = "http://127.0.0.1:8000/api";

import * as _api from './api.js?version=0.95'
import * as _init from './init.js?version=0.9'
import * as _utils from './utils.js?version=0.58'
import * as _quartile from './quartile.js?version=1'
import * as _popMap from './population_map.js?version=0.6'
import * as _popPyramid from './population_pyramid.js?version=0.1'
import * as _migrationProb from './migration_probabilities.js?version=2.047'
import * as _popPprobabilities from './pop_probabilities.js?version=0.19'

//_utils.progressMenuOn();

let txt_Geo_DropDown = "Geo";
let txt_Sex_DropDown = "Sex";
let txt_Country_Total_Title = "Ukraine totals";
let txt_Title_Top_RightPanel  = "Statistics";
let txt_Title_Bottom_RightPanel  = "Migration";
let txt_Date_Bottom_Panel  = "Date";

//let txt_title_TopLeftPanel_infobox = "Geo";
//let txt_title_BottomLeftPanel_infobox = "Geo";
//let txt_title_TopRightPanel_infobox = "Geo";
//let txt_title_BottomRightPanel_infobox = "Geo";

let root_plotChordDiagramt = null;
let series_plotChordDiagramt = null;

am5.ready(function () {
    root_plotChordDiagramt = am5.Root.new("plotChordDiagram");
    series_plotChordDiagramt = _init.initialise_migration_probabilities_chart(root_plotChordDiagramt);
});

let root_plotChordDiagramt_LG = null;
let series_plotChordDiagramt_LG = null;

am5.ready(function () {
    root_plotChordDiagramt_LG = am5.Root.new("plotChordDiagram_lg");
    series_plotChordDiagramt_LG = _init.initialise_migration_probabilities_chart_LG(root_plotChordDiagramt_LG);
});


let root_PopulationPyramid = null;
let series_PopulationPyramid = null;

am5.ready(function () {
    root_PopulationPyramid = am5.Root.new("plotPopulationPyramid");
    series_PopulationPyramid = _init.initialise_PopulationPyramid_chart(root_PopulationPyramid);
});

let palette_population = ["#f7fbff", "#e9f2f9", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b"];
var country_ISO3 = "";
var accessToken = "null";
var admin_level = 1;
var admin_pcode = null;
var admin_name_en = null;

localStorage.setItem('access', "false");
localStorage.setItem('token', "null");



let initialCountries = _init.getCountries(API_URL);


var country_ISO3 = initialCountries[0].country;
var initBounding_box = initialCountries[0].bounding_box;
var initBounding_centroid = initialCountries[0].centroid;

let initialData = _init.getInitData(API_URL, country_ISO3);

let admin_units_geo = _utils.get_admin_units_geo(API_URL, country_ISO3, admin_level);

var admin_names = _utils.get_admin_names(initialData);
var admin_names_total_count = Object.keys(admin_names).length;

_utils.setGeoMenu(admin_names);

console.log(initialData);

var age_ranges_available = _utils.getAgeRanges(initialData);

console.log(age_ranges_available);

var age_min_selected = initialData.age_ranges[0]["age_min"];
var age_max_selected = initialData.age_ranges[initialData.age_ranges.length - 1]["age_max"];

console.log(age_max_selected);


var dates_available = _utils.getDates(initialData.dates_pop);
var date_selected = dates_available[dates_available.length - 1];

var migrationProb = null;
var migrationProbRank_by = "count";


var basemaps = {
    "OpenStreetMaps": L.tileLayer(
            "https://cartodb-basemaps-b.global.ssl.fastly.net/light_nolabels/{z}/{x}/{y}.png", {
                attribution: "<a href='http://www.esri.com/'>Esri</a>, HERE, Garmin, (c) OpenStreetMap",
                minZoom: 1,
                maxZoom: 14,
                id: "osm.streets"
            }
    )
};


var mapOptions = {
    zoomControl: false,
    attributionControl: false,
    center: [48.383022, 31.1828699],
    zoom: 1,
    maxZoom: 11,
    layers: [basemaps.OpenStreetMaps]
};

var map = L.map("map", mapOptions);
map.invalidateSize();

var CopyrightLayer = L.control({position: 'bottomleft'});

CopyrightLayer.onAdd = function (map) {
    var div = L.DomUtil.create('div', 'Copyright_data');
    div.innerHTML += '<div id="Copyright_info"><p class="pt-2"><small>&nbsp;&copy; 2024 Ukraine Pop - <a href="#" style="text-decoration: none;"> Website built by Oxford</a></small></p></div>';
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
};

CopyrightLayer.addTo(map);

var scaleLayer = L.control.scale({position: 'bottomleft'});
scaleLayer.addTo(map);


L.control.zoom({
    position: 'bottomleft'
}).addTo(map);

// create the control Probability
var controlPanel_Probability_Demographic  = L.control({position: 'topright'});

controlPanel_Probability_Demographic.onAdd = function () {
    this._div = L.DomUtil.get('controlPanel_TopRightID');
    return this._div;
};
controlPanel_Probability_Demographic.addTo(map);


// create the control population pyramid
var controlPanel_Migration_Flow = L.control({position: 'topright'});

controlPanel_Migration_Flow.onAdd = function (map) {
    this._div = L.DomUtil.get('controlPanel_BottomRightID');
    return this._div;
};
controlPanel_Migration_Flow.addTo(map);




// create the control Panel
var controlPanel_Controls = L.control({position: 'topleft'});

controlPanel_Controls.onAdd = function (map) {
    this._div = L.DomUtil.get('controlPanel_ControlsID');
    return this._div;
};
controlPanel_Controls.addTo(map);



// create the control Chord Diagram
var controlPanel_Info = L.control({position: 'topleft'});

controlPanel_Info.onAdd = function (map) {
    this._div = L.DomUtil.get('controlPanel_BottomleftID');
    return this._div;
};
controlPanel_Info.addTo(map);
//console.log((controlPanel_Info._map));

// create the control
var controlPanel_DateTime = L.control({position: 'bottomcenter'});

controlPanel_DateTime.onAdd = function (map) {
    this._div = L.DomUtil.get('controlPanel_DateTimeID');
    return this._div;
};
controlPanel_DateTime.addTo(map);


map.createPane('labels');
map.getPane('labels').style.zIndex = 650;
map.getPane('labels').style.pointerEvents = 'none';
var cartocdn = L.tileLayer('https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png', {
    pane: 'labels'
}).addTo(map);


var popupCode = L.popup({
    closeButton: false,
    className : 'another-popup',
    maxWidth: 300,
    minWidth: 100
});



var layerCountry = L.geoJson(null, {
    style: {

    },
    onEachFeature: function (feature, layer) {
      
    layer.bindPopup(popupCode);
    
        layer.on({
            mousemove: function (e) {
              layer.openPopup(e.latlng);
            },
            mouseover: function (e) {
                
            layer.getPopup().setContent('<p class="m-0 p-0"><b>'+feature.properties.name_en+'</b><br/>code: '+feature.properties.pcode+'</p>');  
            layer.getPopup().update();
            _popMap.highlightFeaturePopulationMap(e, map);

            },
            mouseout: function (e) {
                layer.closePopup();
                _popMap.resetFeaturePopulationMap(e, map);
//                $("#popup-" + e.target.feature.properties.fid).remove();
            },
            click: function (e) {
                
                admin_pcode = e.target.feature.properties.pcode;
                admin_name_en = e.target.feature.properties.name_en;
                
                main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);

                document.getElementById('controlPanel_BottomleftID').style.height = '400px';
                document.getElementById('controlPanel_InfoBoxCode').style.visibility = 'visible';
                document.getElementById('adminTotalLabel').innerHTML = e.target.feature.properties.population_totals;
                document.getElementById('adminNameLabel').innerHTML =  admin_name_en; 
                document.getElementById('adminPCodeLabel').innerHTML =  admin_pcode; 
                document.getElementById('controlPanel_BottomRightID_label').innerHTML =  txt_Title_Bottom_RightPanel + " to/from [ " + admin_pcode +" ]";
                document.getElementById('controlPanel_TopRightID_label').innerHTML =  txt_Title_Top_RightPanel + " [ " + admin_pcode +" ]"; 
                
                var selGEO = document.getElementById("idSelectGeoLevel");
                document.getElementById('infoGEOLabel').innerHTML = selGEO.options[selGEO.selectedIndex].text;
            }
        });

    }.bind(this)
}).addTo(map);




var legendPopMap = L.control({position: 'bottomright'});

legendPopMap.onAdd = function (map) {
    var legent_text = "legend ";
    var div = L.DomUtil.create('div', 'legend_PopMap');
    div.innerHTML += '<div id="legend_PopMap_info"></div>';
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
};
legendPopMap.addTo(map);


// Set a range slider
$(".slider_age_selections")
        .slider({
            min: 0,
            max: age_ranges_available[0].length - 1,
            range: true,
            values: [0, age_ranges_available[0].length - 1]
        })
//        .slider("float", {
//            rest: "label",
//            labels: age_ranges_available,
//            formatLabel: function(e){ 
//                 return(e);
//            }
//        })
        .slider("pips", {
            labels: {first: "0", last: age_ranges_available[1][age_ranges_available[1].length - 1] + "+"}
        }).on("slide", function (e, ui) {
               
            if (ui.values[1] < ui.values[0]) {
             return false;
            }
//            let age_min = age_ranges_available[0][ui.values[0]];    
//            let age_max = age_ranges_available[1][ui.values[1]];
//            if (ui.values[1] === age_ranges_available[1].length-1){
//                age_max = age_max + "+";
//            }
            //document.getElementById('label_Age_range').innerHTML = "Age [ min: "+ age_min +" max: "+age_max+" ]";   
            _utils.update_Age_Range_labele(ui.values[0], ui.values[1], age_ranges_available);
      
        }).on("slidestop", function (e, ui) {
            if (e.originalEvent) {
                main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);
            }            
        });
   


$(".Date-slider")
        .slider({
            min: 0,
            max: dates_available.length - 1,
            value: dates_available.length - 1
        })
        .slider("float", {
            labels: dates_available
        })
        .slider("pips", {
            rest: "label",
            labels: dates_available,
            step: 10
        })
        .on("slidechange", function (e, ui) {
            if (e.originalEvent) {
                main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);
            }
        });



$("#btnExpand_ChordDiagrams").on("click", function () {

     // when open a big windows start shoing animation
     series_plotChordDiagramt_LG.bullets.push(function (_root, _series, dataItem) {
        var bullet = am5.Bullet.new(root_plotChordDiagramt_LG, {
            locationY: Math.random(),
            sprite: am5.Circle.new(root_plotChordDiagramt_LG, {
                radius: 5,
                fill: dataItem.get("source").get("fill")
            })
        });

        bullet.animate({
            key: "locationY",
            to: 1,
            from: 0,
            duration: Math.random() * 1000 + 2000,
            loops: Infinity
        });

        return bullet;
    });  
    
    $('#idMdPlotChordDiagram').modal('show');
});


 $("#idMdPlotChordDiagram").on('hide.bs.modal', function () {
     series_plotChordDiagramt_LG.bulletsContainer.children.clear();
     series_plotChordDiagramt_LG.bullets.clear();
 });


$('#plotChordDiagramDisplaySelect input').on("click", function () {
    
    
    main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);

    if (this.id === "btnradioPlotChordDiagramCount") {
        
        migrationProbRank_by = "count";
        document.getElementById('idMdPlotChordDiagram_lable').innerHTML = "Count";

    } else if (this.id === "btnradioPlotChordDiagramProbability") {

        migrationProbRank_by = "probability";
        document.getElementById('idMdPlotChordDiagram_lable').innerHTML = "Probability";
    }
    
});




$("#fSignin").on("click", function () {
    
    $('#passwordsNoMatchRegister').hide();

    let Username = document.getElementById("fInputUsername").value;
    let Password = document.getElementById("fInputPassword").value;

    let result = _api.get_sign_in(API_URL, Username, Password);

    if (result.status === 200) {
        accessToken = result.access_token;
        localStorage.setItem('access', "true");
        localStorage.setItem('token', accessToken);
        _utils.setGeoMenu(admin_names);
        $("#fInputUsername").val("");
        $("#fInputPassword").val("");
        document.getElementById("idFormToLogin").style.display = "none";
        document.getElementById("idMdLoginFormHeader").style.display = "none";
        $('#idSuccessfullyloggedin').show();
        
        _utils.resetSlider_age_selections(age_ranges_available);
        _utils.resetDate_slider(dates_available);
    }
});


$("#idSuccessfullyloggedinContinue").on("click", function () {

    document.getElementById("idFormToLogin").style.display = "block";
    document.getElementById("idMdLoginFormHeader").style.display = "block";
    $('#passwordsNoMatchRegister').hide();
    $('#idSuccessfullyloggedin').hide();
    $("#fInputUsername").val("");
    $("#fInputPassword").val("");
    $('#idMdLoginForm').modal('hide');

});



function main_get_pop_migration(api_url, country, admin_level = 1, admin_id = null, dates_available, age_ranges_available, accessToken) {
    
    let result_pyramid;
    
    _utils.progressMenuOn();

    _api.get_population(api_url,
            country,
            admin_level,
            admin_id,
            dates_available,
            age_ranges_available,
            accessToken).then(result => {
               
         if (typeof admin_id !== "undefined" && admin_id !== null) {
             result_pyramid = result["pyramid_"+admin_id];
             document.getElementById('adminFemaleTotalLabel').innerHTML = _utils.sumMaleFemaleInAdmin(result_pyramid.female_population);
             document.getElementById('adminMaleTotalLabel').innerHTML = _utils.sumMaleFemaleInAdmin(result_pyramid.male_population);
         }else{
             result_pyramid = result["pyramid_"];
         }
//         console.log(result_pyramid);
//         console.log(initialData.age_ranges);
        _popPyramid.updatePopulationPyramid(result_pyramid, series_PopulationPyramid, initialData.age_ranges);
        _popPprobabilities.update_pop_probabilities(result.density_plots);
        _popMap.updatePopulationMap(map, layerCountry, admin_units_geo, result.population_totals, palette_population);
        
        document.getElementById('cntrlTotalLabel').innerHTML = _utils.sumNumbersInJSON(result.population_totals);
        document.getElementById('cntrlTotalFemalesLabel').innerHTML = _utils.sumFemaleInJSON(result.population_totals_by_sex);
        document.getElementById('cntrlTotalMalesLabel').innerHTML = _utils.sumMaleInJSON(result.population_totals_by_sex);


        _api.get_migration_probabilities(api_url,
                country,
                admin_level,
                admin_id,
                dates_available,
                age_ranges_available,
                accessToken).then(result => {

            // ifg no data for migration
            if (_utils.isObjectEmpty(result)){
             
                document.getElementById('controlPanel_BottomRightID').style.visibility = 'hidden';
                document.getElementById('adminMigrationInLabel').innerHTML = "None";
                document.getElementById('adminMigrationOutLabel').innerHTML = "None";
                document.getElementById('adminMigrationTotalLabel').innerHTML = "None";
                
            }else{

             document.getElementById('controlPanel_BottomRightID').style.visibility = 'visible';   
            _migrationProb.update_total_in_out_for_admin(admin_id, result);
            _migrationProb.update_migration_probabilities(result, series_plotChordDiagramt);
            //_migrationProb.update_migration_probabilities_LG(result, series_plotChordDiagramt_LG);
            
             series_plotChordDiagramt_LG.bulletsContainer.children.clear();
             series_plotChordDiagramt_LG.bullets.clear();
            _migrationProb.update_migration_probabilities_LG(result, series_plotChordDiagramt_LG, root_plotChordDiagramt_LG);
            
            }
            

            

        }).then(() => {
            // _utils.progressMenuOff();
            //console.log("get_migration_probabilities completed");
        }).catch(error => {
            console.log('In the catch', error);
        });


    }).then(() => {
        _utils.progressMenuOff();
        //console.log("Initial request and initilisation during start up of the page");
    }).catch(error => {
        console.log('In the catch', error);
        alert(error.responseText);
        _utils.progressMenuOff();
    });

}


main_get_pop_migration(API_URL, country_ISO3, admin_level, null, dates_available, age_ranges_available,  accessToken);


// ****************************************************************************            


map.fitBounds(L.geoJson(admin_units_geo).getBounds());


L.DomEvent.disableClickPropagation(controlPanel_TopRightID);
L.DomEvent.disableScrollPropagation(controlPanel_TopRightID);
L.DomEvent.disableClickPropagation(controlPanel_ControlsID);
L.DomEvent.disableScrollPropagation(controlPanel_ControlsID);
L.DomEvent.disableClickPropagation(controlPanel_BottomRightID);
L.DomEvent.disableScrollPropagation(controlPanel_BottomRightID);
L.DomEvent.disableClickPropagation(controlPanel_BottomleftID);
L.DomEvent.disableScrollPropagation(controlPanel_BottomleftID);
L.DomEvent.disableClickPropagation(controlPanel_DateTimeID);
L.DomEvent.disableScrollPropagation(controlPanel_DateTimeID);

_utils.update_Age_Range_labele(0, age_ranges_available[0].length-1, age_ranges_available);

$('#SwitchAnimationMigrationPlot_LG').change(function() {
   if ($(this).prop('checked')){
       series_plotChordDiagramt_LG.bulletsContainer._display.visible = true;
   }else{
       series_plotChordDiagramt_LG.bulletsContainer._display.visible = false;
   }
});


$( "#btnResetSelectedAdmin_cros" ).on( "click", function() {
    document.getElementById('adminTotalLabel').innerHTML = "";
    document.getElementById('adminNameLabel').innerHTML =  ""; 
    document.getElementById('controlPanel_InfoBoxCode').style.visibility = 'hidden';
    document.getElementById('controlPanel_BottomleftID').style.height = '160px';
    admin_pcode = null;   
    main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);
    document.getElementById('controlPanel_BottomRightID_label').innerHTML =  txt_Title_Bottom_RightPanel;
    document.getElementById('controlPanel_TopRightID_label').innerHTML =  txt_Title_Top_RightPanel; 
 
});

$( "#btnResetAllSettingsSelected" ).on( "click", function() {
    document.getElementById('adminTotalLabel').innerHTML = "";
    document.getElementById('adminNameLabel').innerHTML =  ""; 
    document.getElementById('controlPanel_InfoBoxCode').style.visibility = 'hidden';
    document.getElementById('controlPanel_BottomleftID').style.height = '160px';
    admin_pcode = null;
    _utils.resetSlider_age_selections(age_ranges_available);
    _utils.resetDate_slider(dates_available);
    document.getElementById("idSelectSex").selectedIndex = 0;
    document.getElementById("idSelectGeoLevel").selectedIndex = 0;
    admin_level=1;
    admin_units_geo = _utils.get_admin_units_geo(API_URL, country_ISO3, admin_level, accessToken);
    main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);
    document.getElementById('controlPanel_BottomRightID_label').innerHTML =  txt_Title_Bottom_RightPanel;
    document.getElementById('controlPanel_TopRightID_label').innerHTML =  txt_Title_Top_RightPanel; 
 
});



$('#idSelectGeoLevel').change(function() {
    
    document.getElementById('adminTotalLabel').innerHTML = "";
    document.getElementById('adminNameLabel').innerHTML =  ""; 
    document.getElementById('controlPanel_InfoBoxCode').style.visibility = 'hidden';
    document.getElementById('controlPanel_BottomleftID').style.height = '160px';
    _utils.resetSlider_age_selections(age_ranges_available);
    _utils.resetDate_slider(dates_available);
    document.getElementById("idSelectSex").selectedIndex = 0;
    admin_pcode = null;
    admin_level = $(this).val();
    admin_units_geo = _utils.get_admin_units_geo(API_URL, country_ISO3, $(this).val(), accessToken);
     
    main_get_pop_migration(API_URL, country_ISO3, admin_level, null, dates_available, age_ranges_available,  accessToken);
    document.getElementById('controlPanel_BottomRightID_label').innerHTML =  txt_Title_Bottom_RightPanel;
    document.getElementById('controlPanel_TopRightID_label').innerHTML =  txt_Title_Top_RightPanel; 
});


$('#idSelectSex').change(function() {
    main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);
});

$( "#btnSettings" ).on( "click", function() {
    $('#idMdSettings').modal('show');
});


$( "#btnQuestionPanel_Controls" ).on( "click", function() {
    $('#idMdHelpMainControle').modal('show');
});

$( "#btnQuestionTopRight_panel" ).on( "click", function() {
    $('#idMdHelpTopRightPanel').modal('show');
});

$( "#btnQuestionBottomRight_panel" ).on( "click", function() {
    $('#idMdHelpBottomRightPanel').modal('show');
});

$( "#btnQuestionBottomLeft_panel" ).on( "click", function() {
    $('#idMdHelpBottomLeftPanel').modal('show');
});

$( "#btnLogin" ).on( "click", function() {
    $('#idMdHelpMainControle').modal('hide');
    $('#idMdLoginForm').modal('show');
});


_utils.update_Panels_labels(txt_Geo_DropDown, 
                            txt_Sex_DropDown, 
                            txt_Country_Total_Title, 
                            txt_Date_Bottom_Panel);


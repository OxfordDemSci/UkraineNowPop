var API_URL = "http://127.0.0.1:8000/api";

import * as _api from './api.js?version=0.5'
import * as _init from './init.js?version=0.4'
import * as _utils from './utils.js?version=0.46'
import * as _quartile from './quartile.js?version=1'
import * as _popMap from './population_map.js?version=0.6'
import * as _popPyramid from './population_pyramid.js?version=0.1'
import * as _migrationProb from './migration_probabilities.js?version=1.92'
import * as _popPprobabilities from './pop_probabilities.js?version=0.17'

//_utils.progressMenuOn();

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

var age_ranges_available = _utils.getAgeRanges(initialData);

var age_min_selected = initialData.age_ranges[0]["age_min"];
var age_max_selected = initialData.age_ranges[initialData.age_ranges.length - 1]["age_max"];

var dates_available = _utils.getDates(initialData.dates);
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
var controlPanel_Probability = L.control({position: 'topright'});

controlPanel_Probability.onAdd = function () {
    this._div = L.DomUtil.get('controlPanel_ProbabilityID');
    return this._div;
};
controlPanel_Probability.addTo(map);

// create the control Panel
var controlPanel_Controls = L.control({position: 'topleft'});

controlPanel_Controls.onAdd = function (map) {
    this._div = L.DomUtil.get('controlPanel_ControlsID');
    return this._div;
};
controlPanel_Controls.addTo(map);


// create the control population pyramid
var controlPanel_PopulationPyramid = L.control({position: 'topright'});

controlPanel_PopulationPyramid.onAdd = function (map) {
    this._div = L.DomUtil.get('controlPanel_PopulationPyramidID');
    return this._div;
};
controlPanel_PopulationPyramid.addTo(map);

// create the control Chord Diagram
var controlPanel_ChordDiagram = L.control({position: 'topleft'});

controlPanel_ChordDiagram.onAdd = function (map) {
    this._div = L.DomUtil.get('controlPanel_ChordDiagramID');
    return this._div;
};
controlPanel_ChordDiagram.addTo(map);


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


var layerCountry = L.geoJson(null, {
    style: {

    },
    onEachFeature: function (feature, layer) {

        layer.on({
            mouseover: function (e) {
                _popMap.highlightFeaturePopulationMap(e, map);

                var popup = $("<div></div>", {
                    id: "popup-" + e.target.feature.properties.fid,
                    css: {
                        position: "absolute",
                        top: "60px",
                        left: "280px",
                        zIndex: 1002,
                        backgroundColor: "white",
                        padding: "8px",
                        border: "1px solid #ccc"
                    }
                });
                var hedName = $("<div></div>", {
                    html: "Name: <b> " + e.target.feature.properties.name_en + "</b><br/>Code : <b>"+ e.target.feature.properties.pcode + "</b><br/>Population: <b>"+e.target.feature.properties.population_totals+"<b>",
                    css: {fontSize: "14px", marginBottom: "3px"}
                }).appendTo(popup);
                popup.appendTo("#map");

            },
            mouseout: function (e) {
                _popMap.resetFeaturePopulationMap(e, map);
                $("#popup-" + e.target.feature.properties.fid).remove();
            },
            click: function (e) {
                
                admin_pcode = e.target.feature.properties.pcode;
                admin_name_en = e.target.feature.properties.name_en;
                
                main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);
                
                document.getElementById('adminTotalInfoBox').style.visibility = 'visible';
                document.getElementById('adminTotalLabel').innerHTML = admin_pcode + " total : " + e.target.feature.properties.population_totals;
                document.getElementById('adminNameLabel').innerHTML =  "Name : " + admin_name_en; 
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
            max: age_ranges_available.length - 1,
            range: true,
            values: [0, age_ranges_available.length - 1]
        })
        .slider("float", {
            labels: age_ranges_available
        })
        .slider("pips", {
            labels: {first: age_ranges_available[0].toString(), last: age_ranges_available[age_ranges_available.length - 1] + "+"}
        }).on("slide", function (e, ui) {
    if (ui.values[1] === ui.values[0]) {
        return false;
    }
})
        .on("slidestop", function (e, ui) {

            main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);
            
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

            main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);

        });



$("#btnExpand_ChordDiagrams").on("click", function () {
    $('#idMdPlotChordDiagram').modal('show');
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


$( "#btnSettings" ).on( "click", function() {
    $('#idMdSettings').modal('show');
});



// ****************************************************************************
// ****************************************************************************
// Initial request and initilisation during start up of the page
// ****************************************************************************
// ****************************************************************************

//_api.get_population(API_URL,
//        country_ISO3,
//        admin_level,
//        null,
//        dates_available,
//        age_ranges_available,
//        accessToken).then(result => {
//
//        _popPyramid.updatePopulationPyramid(result.pyramid_, series_PopulationPyramid, initialData.age_ranges);
//        _popPprobabilities.update_pop_probabilities(result.pop_posteriors);
//        _popMap.updatePopulationMap(map, layerCountry, admin_units_geo, result.population_totals, palette_population);
//        document.getElementById('cntrlTotalLabel').innerHTML = _utils.sumNumbersInJSON(result.population_totals);
//        
//            _api.get_migration_probabilities(API_URL, 
//                                             country_ISO3, 
//                                             admin_level, 
//                                             null, 
//                                             dates_available, 
//                                             age_ranges_available, 
//                                             migrationProbRank_by,
//                                             accessToken).then(result => {
//                                                 
//            migrationProb=result;
//            
//            _migrationProb.update_migration_probabilities(result, series_plotChordDiagramt);
//            _migrationProb.update_migration_probabilities_LG(result, series_plotChordDiagramt_LG);
//
//            }).then(() => {
//            // _utils.progressMenuOff();
//            console.log("get_migration_probabilities completed");
//            }).catch(error => {
//                console.log('In the catch', error);
//            });
//            
//
//}).then(() => {
//    _utils.progressMenuOff();
//    console.log("Initial request and initilisation during start up of the page");
//}).catch(error => {
//    console.log('In the catch', error);
//});

function main_get_pop_migration(api_url, country, admin_level = 1, admin_id = null, dates_available, age_ranges_available, accessToken = "null") {

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
         }else{
             result_pyramid = result["pyramid_"];
         }
         
        _popPyramid.updatePopulationPyramid(result_pyramid, series_PopulationPyramid, initialData.age_ranges);
        _popPprobabilities.update_pop_probabilities(result.pop_posteriors);
        _popMap.updatePopulationMap(map, layerCountry, admin_units_geo, result.population_totals, palette_population);
        document.getElementById('cntrlTotalLabel').innerHTML = _utils.sumNumbersInJSON(result.population_totals);

        _api.get_migration_probabilities(api_url,
                country,
                admin_level,
                admin_id,
                dates_available,
                age_ranges_available,
                accessToken).then(result => {

            _migrationProb.update_migration_probabilities(result, series_plotChordDiagramt);
            _migrationProb.update_migration_probabilities_LG(result, series_plotChordDiagramt_LG);

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
    });

}


main_get_pop_migration(API_URL, country_ISO3, admin_level, null, dates_available, age_ranges_available,  accessToken);


// ****************************************************************************            


map.fitBounds(L.geoJson(admin_units_geo).getBounds());


L.DomEvent.disableClickPropagation(controlPanel_ProbabilityID);
L.DomEvent.disableScrollPropagation(controlPanel_ProbabilityID);
L.DomEvent.disableClickPropagation(controlPanel_ControlsID);
L.DomEvent.disableScrollPropagation(controlPanel_ControlsID);
L.DomEvent.disableClickPropagation(controlPanel_PopulationPyramidID);
L.DomEvent.disableScrollPropagation(controlPanel_PopulationPyramidID);
L.DomEvent.disableClickPropagation(controlPanel_ChordDiagramID);
L.DomEvent.disableScrollPropagation(controlPanel_ChordDiagramID);
L.DomEvent.disableClickPropagation(controlPanel_DateTimeID);
L.DomEvent.disableScrollPropagation(controlPanel_DateTimeID);


$('#SwitchAnimationMigrationPlot_LG').change(function() {
   if ($(this).prop('checked')){
       series_plotChordDiagramt_LG.bulletsContainer._display.visible = true;
   }else{
       series_plotChordDiagramt_LG.bulletsContainer._display.visible = false;
   }
});

$( "#btnResetSelectedAdmin" ).on( "click", function() {
    
    document.getElementById('adminTotalLabel').innerHTML = "";
    document.getElementById('adminNameLabel').innerHTML =  ""; 
    document.getElementById('adminTotalInfoBox').style.visibility = 'hidden';
    admin_pcode = null;
    main_get_pop_migration(API_URL, country_ISO3, admin_level, admin_pcode, dates_available, age_ranges_available,  accessToken);
    
});
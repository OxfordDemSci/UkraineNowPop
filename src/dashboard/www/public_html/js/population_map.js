import * as _utils from './utils.js?version=2.3'
import * as _quartile from './quartile.js?version=1'
import * as _ImageFromRGB from './createImageFromRGBdata.js?version=1'


function getPalettePopMap(data, palette_colors) {
  
let vls = [];
let cnt = 0;
for (var key in data) {

           vls.push(
                data[key]
            );
           cnt = 0;
       
}
  
    vls.sort(function(a, b) {
        return a - b;
    });
    let all_count = (vls);
       
    let breaks = [];
    let cont = 1;
    let Quartile;
    
    for (var i = 0; i < 10; i++) {
       Quartile = Math.round(_quartile.Quartile(all_count, cont*0.1));
       breaks.push(Quartile);
        cont++;  
    }  
    
    let breaks_unique = uniqueArray(breaks);
    

    let palette_final = [];
    let diference = palette_colors.length-breaks_unique.length;
    for (var i = 0; i < breaks_unique.length; i++) {
             palette_final.push(
                    palette_colors[i+diference]
            );
    }  
    
    let palette = {"breaks":breaks_unique, "colors":palette_final};

    return palette;
 
}


export function getColor(v, palette) {

    if (v === undefined || v === null) {
        return "#EBEBE4";
    }

    let xcase = false;
    let color;
    let breaks = palette["breaks"];
    let colors = palette["colors"];
    let lp = breaks.length;

    if (lp === 1) {
        return colors[0];
    }

    if (breaks[lp - 2] === breaks[lp - 1]) {
        lp--;
        xcase = true;
    }

    for (let i = 0; i < lp - 1; i++) {

        if (v >= breaks[i] && v <= breaks[i + 1]) {
            color = colors[i];
        }
    }

    if (xcase && v >= breaks[lp - 1]) {
        color = colors[lp - 1];
    }

    if (v < breaks[0]) {
        color = colors[0];
    }

    return color;

}

export function RestyleLayerPopMapOpacity(_layer) {
    
    
    let Opacity =  document.getElementById("customRangeOpacity").value; 
        
    _layer.eachLayer(function(featureInstanceLayer) {
      
                    featureInstanceLayer.setStyle({
                        fillOpacity: Opacity
                    });
  
        
        
    });
    
}

export function RestyleLayerPopMap(_layer, palette, _admin_pcode) {
    
    let propertyName = "population_totals";
    let Opacity =  document.getElementById("customRangeOpacity").value; 
        
    _layer.eachLayer(function(featureInstanceLayer) {
       var propertyValue = featureInstanceLayer.feature.properties[propertyName];
        

        if (propertyValue == undefined || propertyValue == null) {
            
            featureInstanceLayer.setStyle({
                fillColor: "#EBEBE4",
                fillOpacity: 0,
                color: "#EBEBE4",
                weight: 0
            });           
        }else{

            var mFillColor = getColor(propertyValue, palette);
            
                if (_admin_pcode === featureInstanceLayer.feature.properties["pcode"]){
                    featureInstanceLayer.setStyle({
                        fillColor: mFillColor,
                        fillOpacity: Opacity,
                        color: "black",
                        weight: 2
                    });
                }else{
                    featureInstanceLayer.setStyle({
                        fillColor: mFillColor,
                        fillOpacity: Opacity,
                        color: "black",
                        weight: 0.3
                    });
                }   

        }
        
        
    });
}

export function uniqueArray(arr) {
    var a = [];
    for (var i=0, l=arr.length; i<l; i++)
        if (a.indexOf(arr[i]) === -1 && arr[i] !== '')
            a.push(arr[i]);
    return a;
}


export function updatePopulationMap(_map, _layer, geoJson, data, palette_colors, title, _admin_pcode) {
    
   _layer.clearLayers();
   _map.removeLayer(_layer);
    let cnt = 0;
    
    for (var i = 0; i < geoJson.features.length; i++) {
            var pcode = geoJson.features[i].properties.pcode;
            geoJson.features[i].properties.population_totals=data[pcode];
    }    

   _layer.addTo(_map);
   _layer.addData(geoJson); 
   
   let palette  = getPalettePopMap(data, palette_colors);
   RestyleLayerPopMap(_layer, palette, _admin_pcode);
    
   loadLagentPopMap(title, palette["colors"], palette["breaks"], "subtitles");
    
    const resizeObserver = new ResizeObserver(() => {
        _map.invalidateSize();
    });

    //resizeObserver.observe(document.getElementById("map_uk"));
    
    
}


export function loadLagentPopMap(title, colors, breaks, subtitles) {

    var html = '<div style="width:80px"><p>' + title + '</p></div>';
    
    var subtitlesArray = Array(colors.length).fill('');
    subtitlesArray[0] = subtitles[0];
    subtitlesArray[colors.length-1] = subtitles[1];    
    
//    html += '<div style="width:100px">' + subtitles[0] + '</div>';
    
    html += '<ul style="list-style-type: none;margin-top: 2px;margin-bottom: 2px;padding-inline-start: 10px;">';
    for (var i = 0, len = colors.length; i < len; i++) {
        var rgb = _ImageFromRGB.hexToRGB(colors[i]);
        var mCanvas = _ImageFromRGB.createImageFromRGBdata(rgb.r, rgb.g, rgb.b, 20, 20);

        html += '<li><img width="16px" height="16px" src="' + mCanvas.toDataURL() + '"><span>&#32;&#32;&#32;&#32; &nbsp;&nbsp;' + _utils.nFormatter_Space(breaks[i], 1) + '</span></li>';
    }
    html += '</ul>';

    document.getElementById('legend_PopMap_info').innerHTML = html;

}

export function highlightFeaturePopulationMap(e,  _map) {

    var layer = e.target;
    
    var Opacity = 0.7;
    
    if (_map) {
  
        let country = e.target.feature.properties.country;
        if (typeof country !== 'undefined' && country !== null) {
            layer.setStyle({
                weight: 1,
                fillOpacity: Opacity
            });
        }

    }

}

export function highlightFeaturePopulationMapSelected(e,  _map) {

    var layer = e.target;
    
    var Opacity =  document.getElementById("customRangeOpacity").value;
    
    if (_map) {
  
        let country = e.target.feature.properties.country;
        if (typeof country !== 'undefined' && country !== null) {
            layer.setStyle({
                weight: 2,
                fillOpacity: Opacity
            });
        }
    }

}


export function resetFeaturePopulationMap(e, _map, _admin_pcode) {
    
    var layer = e.target;
    
    var Opacity =  document.getElementById("customRangeOpacity").value;
    
    if (_admin_pcode !== e.target.feature.properties.pcode) {
        
        layer.setStyle({
            weight: 0.5,
            fillOpacity: Opacity
        });

    }
//    let country = e.target.feature.properties.country;
//    console.log(country);
//    if (typeof country === 'undefined' && country === null) {
//        //_map.removeControl(_infoBox);   
//        //control.removeFrom(map);
//    }
    
    
}


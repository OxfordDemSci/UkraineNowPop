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
    
    // include actual min and max boundaries to form intervals
    breaks.unshift(vls[0]);              // min
    breaks.push(vls[vls.length - 1]);   // max

    // unique & sorted numeric breaks
    const breaks_unique = Array.from(new Set(breaks)).sort((a, b) => a - b);

    // the number of intervals is breaks_unique.length - 1
    const intervals = Math.max(1, breaks_unique.length - 1);

    // palette_colors: last element is NA color, the rest are candidates
    const baseColors = palette_colors.slice(0, palette_colors.length - 1);    

     let palette_final;
        const start = baseColors.length - intervals;
        palette_final = baseColors.slice(start);
 

    return {
        breaks: breaks_unique,
        colors: palette_final,
        naColor: palette_colors[palette_colors.length - 1]
    };
 
}


export function getColor(v, palette) {

    if (v === undefined || v === null) {
        return palette.naColor;
    }
    const val = +v;
    const breaks = palette.breaks;
    const colors = palette.colors;


    // iterate intervals: [breaks[i], breaks[i+1]) except include last as >=
    for (let i = 0; i < breaks.length - 1; i++) {
        const low = breaks[i];
        const high = breaks[i + 1];
        if (i < breaks.length - 2) {
            // all but final interval: inclusive low, exclusive high
            if (val >= low && val < high) return colors[i];
        } else {
            // last interval: inclusive both sides (to include max)
            if (val >= low && val <= high) return colors[i];
        }
    }

    // fallback: if smaller than first break
    if (val < breaks[0]) return colors[0];
    // fallback otherwise
    return colors[colors.length - 1] || palette.naColor;
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
        
        var mFillColor = getColor(propertyValue, palette);

        if (propertyValue == undefined || propertyValue == null) {
            
            featureInstanceLayer.setStyle({
                fillColor: mFillColor,
                fillOpacity: Opacity,
                color: "black",
                weight: 2
            });           
        }else{

            
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
    _layer.bringToBack();
   
   let palette  = getPalettePopMap(data, palette_colors);
   RestyleLayerPopMap(_layer, palette, _admin_pcode);
    
   loadLagentPopMap(title, palette);
    
    const resizeObserver = new ResizeObserver(() => {
        _map.invalidateSize();
    });

    //resizeObserver.observe(document.getElementById("map_uk"));
    
    
}


export function loadLagentPopMap(title, palette) {
    const colors = palette.colors.slice().reverse();
    const breaks = palette.breaks.slice().reverse();

    var html = '<div style="width:80px"><p>' + title + '</p></div>';
    
    // var subtitlesArray = Array(colors.length+1).fill('');
    // subtitlesArray[0] = subtitles[0];
    // subtitlesArray[colors.length-1] = subtitles[1];    
    
//    html += '<div style="width:100px">' + subtitles[0] + '</div>';
    // reverse legend order

    html += '<ul style="list-style-type: none;margin-top: 2px;margin-bottom: 2px;padding-inline-start: 10px;">';
    for (var i = 0, len = colors.length; i < len; i++) {
        var rgb = _ImageFromRGB.hexToRGB(colors[i]);
        var mCanvas = _ImageFromRGB.createImageFromRGBdata(rgb.r, rgb.g, rgb.b, 20, 20);

        html += '<li><img width="16px" height="16px" src="' + mCanvas.toDataURL() + '"><span>&#32;&#32;&#32;&#32; &nbsp;&nbsp;' + _utils.nFormatter_Space(breaks[i], 1) + '</span></li>';
    }
        {
        var rgbNA = _ImageFromRGB.hexToRGB(palette.naColor);
        var mCanvasNA = _ImageFromRGB.createImageFromRGBdata(rgbNA.r, rgbNA.g, rgbNA.b, 20, 20);
        html += '<li style="display:flex; align-items:center; margin-top:6px;">' +
                `<img width="16" height="16" src="${mCanvasNA.toDataURL()}" style="margin-right:8px;">` +
                `<span style="font-size:13px;">Data unavailable</span>` +
                '</li>';
    }

    html += '</ul>';

    document.getElementById('legend_PopMap_info').innerHTML = html;

}

export function highlightFeaturePopulationMap(e,  _map) {

    var layer = e.target;
    
    var Opacity = 0.5;
    
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


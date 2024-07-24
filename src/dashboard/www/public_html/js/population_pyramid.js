
export function getPopulationPyramidData(d) {

    let dMale = d.pyramid_.male_population;
    let dFemale = d.pyramid_.female_population;

    return {
        dataMale: dMale.map(function (el) {
            return el.population;
        }),
        dataFemale: dFemale.map(function (el) {
            return el.population;
        })
    };

}

function aggregateData(list, _denominator) {
    var maleTotal = 0;
    var femaleTotal = 0;

    for (var i = 0; i < list.length; i++) {
        var row = list[i];
        maleTotal += row.male;
        femaleTotal += row.female;
    }

    for (var i = 0; i < list.length; i++) {
        var row = list[i];
//        row.malePercent = -1 * Math.round((row.male / maleTotal) * 10000) / 100;
//        row.femalePercent = Math.round((row.female / femaleTotal) * 10000) / 100;

        row.malePercent = -1 * Math.round((row.male / _denominator) * 10000) / 100;
        row.femalePercent = Math.round((row.female / _denominator) * 10000) / 100;
    }

    return list;
}

function getRandomInt(max) {
    return Math.floor(Math.random() * max);
}

//export function updatePopulationPyramid(data, series, initial_age_ranges) {
//
//
//    var values_pyramid = [];
//    
//    for (var i = 0; i < initial_age_ranges.length; i++) {
//        
//        let age_min = initial_age_ranges[i]["age_min"];
//        let age_max = initial_age_ranges[i]["age_max"];
//        
//        var male_population_entry = [];
//        var female_population_entry =[];
//        
//        let SelectedSex = $('#idSelectSex option').filter(":selected").val();
//
//        if (SelectedSex === "Female") {
//            female_population_entry = data.female_population
//                    .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
//                    .map(entry => entry.population);
//
//            male_population_entry = Array(female_population_entry.length).fill(0);
//        }
//        if (SelectedSex === "Male") {
//            
//            male_population_entry = data.male_population
//                    .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
//                    .map(entry => entry.population);
//
//            female_population_entry = Array(male_population_entry.length).fill(0);
//            
//        }
//        if (SelectedSex === "Both") {
//
//            female_population_entry = data.female_population
//                    .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
//                    .map(entry => entry.population);
//
//            male_population_entry = data.male_population
//                    .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
//                    .map(entry => entry.population);
//
//        }        
//  
//        values_pyramid.push(
//                {
//                    age: age_min + "-" + age_max,
//                    male: (male_population_entry.length > 0 ) ? male_population_entry[0] : null,
//                    female: (female_population_entry.length > 0 ) ? female_population_entry[0] : null
//                }
//        );   
//
//    }       
//    
 
//
//
//    values_pyramid = aggregateData(values_pyramid);
//
//    series[2].data.setAll(values_pyramid);
//    series[0].data.setAll(values_pyramid);
//    series[1].data.setAll(values_pyramid);

//  
//    console.log(series[3]);
    
 //   series.set("xAxis").zoomToValues(-10, 10);
            
//    console.log(Math.min(...values_pyramid.map(item => item.malePercent)));
//    console.log(Math.min(...values_pyramid.map(item => item.femalePercent)));
//    
//    const min_m2 = (Math.min(...values_pyramid.map(item => item.malePercent)));
//    const min_f2 = (Math.max(...values_pyramid.map(item => item.femalePercent)));
//    
//    const min_m = Math.ceil(min_m2 / 5) * 5;
//    const min_f = Math.ceil(min_f2 / 5) * 5;
//    
//    series[0].events.once("datavalidated", function(ev) {
//        ev.target.get("xAxis").min=-30;
//    });
//    
    


//
//}

function isEmpty(value){
  return (value == null || value.length === 0);
}


export function updatePopulationPyramid(root, data, initial_age_ranges, _denominator) {

    var values_pyramid = [];
    
    let SelectedSex = $('#idSelectSex option').filter(":selected").val();

    for (var i = 0; i < initial_age_ranges.length; i++) {

        let age_min = initial_age_ranges[i]["age_min"];
        let age_max = initial_age_ranges[i]["age_max"];

        var male_population_entry = [];
        var female_population_entry = [];

        if (SelectedSex === "Female") {
            female_population_entry = data.female_population
                    .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
                    .map(entry => entry.population);

            male_population_entry = Array(female_population_entry.length).fill(0);
        }
        if (SelectedSex === "Male") {

            male_population_entry = data.male_population
                    .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
                    .map(entry => entry.population);

            female_population_entry = Array(male_population_entry.length).fill(0);

        }
        if (SelectedSex === "Both") {

            female_population_entry = data.female_population
                    .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
                    .map(entry => entry.population);

            male_population_entry = data.male_population
                    .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
                    .map(entry => entry.population);

        }

//        values_pyramid.push(
//                {
//                    age: age_min + "-" + age_max,
//                    male: (male_population_entry.length > 0) ? male_population_entry[0] : 0,
//                    female: (female_population_entry.length > 0) ? female_population_entry[0] : 0
//                }
//        );

        if (i === (initial_age_ranges.length-1)){
            values_pyramid.push(
                {
                    age: age_min + " +",
                    male: (male_population_entry.length > 0) ? male_population_entry[0] : 0,
                    female: (female_population_entry.length > 0) ? female_population_entry[0] : 0
                }
            );            
        }else{
            values_pyramid.push(
                {
                    age: age_min + "-" + age_max,
                    male: (male_population_entry.length > 0) ? male_population_entry[0] : 0,
                    female: (female_population_entry.length > 0) ? female_population_entry[0] : 0
                }
            );            
        }

    }


    values_pyramid = aggregateData(values_pyramid, _denominator);
    
    if (SelectedSex === "Female") {
        for (var i = 0; i < values_pyramid.length; i++) {
            values_pyramid[i]["malePercent"]=0;
        }
    }
    
    if (SelectedSex === "Male") {
        for (var i = 0; i < values_pyramid.length; i++) {
            values_pyramid[i]["femalePercent"]=0;
        }
    }

    const max_abs_m = (Math.max(...values_pyramid.map(item => Math.abs(item.malePercent))));
    const max_abs_f = (Math.max(...values_pyramid.map(item => Math.abs(item.femalePercent))));

    let min_fm ;
    let max_fm ;

    if (max_abs_m > max_abs_f) {
        min_fm = -1 * Math.round(max_abs_m);
        max_fm = Math.round(max_abs_m);
    } else {
        min_fm = -1 * Math.round(max_abs_f);
        max_fm = Math.round(max_abs_f);
    }
   

    var femaleSeries;
    var maleSeries;
    var yAxis1;

    am5.ready(function () {

        root.container.children.clear();

        root.setThemes([
            am5themes_Animated.new(root)
        ]);

        var container = root.container.children.push(am5.Container.new(root, {
            layout: root.horizontalLayout,
            width: am5.p100,
            height: am5.p100
        }));

        var chart = container.children.push(am5xy.XYChart.new(root, {
            panX: false,
            panY: false,
            wheelX: "none",
            wheelY: "none",
            layout: root.verticalLayout,
            width: am5.percent(100)
        }));



        yAxis1 = chart.yAxes.push(am5xy.CategoryAxis.new(root, {
            categoryField: "age",
            renderer: am5xy.AxisRendererY.new(root, {
                minorGridEnabled: true,
                minGridDistance: 15
            })
        }));
        yAxis1.get("renderer").grid.template.set("location", 1);
        yAxis1.get("renderer").labels.template.set("fontSize", 11);
        
//        yAxis1.children.moveValue(am5.Label.new(root, { text: "Age group", rotation: -90, y: am5.p50, centerX: am5.p50 }), 0);


        var xAxis = chart.xAxes.push(
                am5xy.ValueAxis.new(root, {
                    min: -1 * min_fm,
                    max: min_fm,
                    numberFormat: "#.s'%'",
                    renderer: am5xy.AxisRendererX.new(root, {
                        minGridDistance: 20
                    })
                })
        );


        xAxis.get("renderer").labels.template.set("fontSize", 11);
//        xAxis.children.push(am5.Label.new(root, { text: "Share of population", x: am5.p50, centerX: am5.p50 }));
        
        maleSeries = chart.series.push(am5xy.ColumnSeries.new(root, {
            name: "Males",
            xAxis: xAxis,
            yAxis: yAxis1,
            valueXField: "malePercent",
            categoryYField: "age",
            clustered: false
        }));

        maleSeries.columns.template.setAll({
            tooltipText: "[fontSize: 12px;]Males, age {categoryY}: {male} ({malePercent.formatNumber('#.0s')}%)",
            tooltipX: am5.p50
        });

        femaleSeries = chart.series.push(am5xy.ColumnSeries.new(root, {
            name: "Males",
            xAxis: xAxis,
            yAxis: yAxis1,
            valueXField: "femalePercent",
            categoryYField: "age",
            clustered: false
        }));

        femaleSeries.columns.template.setAll({
            tooltipText: "[fontSize: 12px;]Femail, age {categoryY}: {female} ({femalePercent.formatNumber('#.0s')}%)",
            tooltipX: am5.p50
        });


        var maleLabel = chart.plotContainer.children.push(am5.Label.new(root, {
            text: "Males",
            fontSize: 12,
            y: 5,
            x: 5,
            //centerX: am5.p50,
            fill: maleSeries.get("fill"),
            background: am5.RoundedRectangle.new(root, {
                fill: am5.color(0xffffff),
                fillOpacity: 0.8
            })
        }));


        var femaleLabel = chart.plotContainer.children.push(am5.Label.new(root, {
            text: "Females",
            fontSize: 12,
            y: 5,
            x: am5.p100,
            centerX: am5.p100,
            dx: -5,
            fill: femaleSeries.get("fill"),
            background: am5.RoundedRectangle.new(root, {
                fill: am5.color(0xffffff),
                fillOpacity: 0.8
            })
        }));

    });

//    values_pyramid = aggregateData(values_pyramid);
//    


    femaleSeries.data.setAll(values_pyramid);
    maleSeries.data.setAll(values_pyramid);
    yAxis1.data.setAll(values_pyramid);


}
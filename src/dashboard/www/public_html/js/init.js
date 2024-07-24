export function getInitData(api_url, country) {

    if (api_url.substr(-1) !== '/')
        api_url += '/';
    const fullUrl = api_url + 'init';

    let data = {};
    data = {
        country: country
    };

    var result = "";
    $.ajax({
        url: fullUrl,
        async: false,
        type: 'get',
        data: data,
        dataType: 'json',
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

export function getCountries(api_url) {

    if (api_url.substr(-1) !== '/')
        api_url += '/';
    const fullUrl = api_url + 'get_countries';


    var result = "";
    $.ajax({
        url: fullUrl,
        async: false,
        type: 'get',
        dataType: 'json',
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


export function initialise_migration_probabilities_chart(root) {

    var series;

    am5.ready(function () {

        root.setThemes([
            am5themes_Animated.new(root)
        ]);


        series = root.container.children.push(am5flow.ChordDirected.new(root, {
            startAngle: 80,
            padAngle: 1,
            linkHeadRadius: 20,
            sourceIdField: "from",
            targetIdField: "to",
            valueField: "value",
            sourceNameField: "fromName",
            targetNameField: "toName",            
            radius: am5.percent(80)
        }));

        series.nodes.get("colors").set("step", 2);
        
//        series.links.template.setAll({
//            tooltipText: "{sourceId} -> {targetId} : {value}"
//        });   
        
        series.links.template.setAll({
            tooltipText: "{fromName} -> {toName} : {value}"
        });             

        series.nodes.labels.template.setAll({
            textType: "radial",
            centerX: 0,
            fontSize: 9,
            maxWidth: 150,
            wrap: true,
            radius: 0
        });

    }); // end am5.ready()

    return(series);
}
// https://www.amcharts.com/docs/v5/charts/flow-charts/chord-diagram/

export function initialise_migration_probabilities_chart_LG(root) {

    var series;

    am5.ready(function () {

        root.setThemes([
            am5themes_Animated.new(root)
        ]);

        series = root.container.children.push(am5flow.ChordDirected.new(root, {
            startAngle: 80,
            padAngle: 1,
            linkHeadRadius: 20,
            sourceIdField: "from",
            targetIdField: "to",
            valueField: "value",
            sourceNameField: "fromName",
            targetNameField: "toName",  
            radius: am5.percent(80)
        }));

        series.nodes.get("colors").set("step", 2);
        
//series.links.template.setAll({
//  tooltipText: "From: {sourceId}\nTo: {targetId}\nValue: {value}"
//});            

        series.links.template.setAll({
            tooltipText: "From: {fromName}\nTo: {toName}\nValue: {value}"
        });     

//        series.bullets.push(function (_root, _series, dataItem) {
//            var bullet = am5.Bullet.new(root, {
//                locationY: Math.random(),
//                sprite: am5.Circle.new(root, {
//                    radius: 5,
//                    fill: dataItem.get("source").get("fill")
//                })
//            });
//
//            bullet.animate({
//                key: "locationY",
//                to: 1,
//                from: 0,
//                duration: Math.random() * 1000 + 2000,
//                loops: Infinity
//            });
//
//            return bullet;
//        });

        series.nodes.labels.template.setAll({
            textType: "radial",
            centerX: 0,
            fontSize: 9,
            maxWidth: 150,
            wrap: true,
            radius: 0
        });

//        series.nodes.bullets.push(function (_root, _series, dataItem) {
//            return am5.Bullet.new(root, {
//                sprite: am5.Circle.new(root, {
//                    radius: 1,
//                    fill: dataItem.get("fill")
//                })
//            });
//        });
//        series.children.moveValue(series.bulletsContainer, 0);

    }); // end am5.ready()


    return(series);
}


export function initialise_PopulationPyramid_chart(root) {

    var femaleSeries;
    var maleSeries;
    var yAxis1;

    am5.ready(function () {

// Set themes
        root.setThemes([
            am5themes_Animated.new(root)
        ]);

// Create wrapper container
        var container = root.container.children.push(am5.Container.new(root, {
            layout: root.horizontalLayout,
            width: am5.p100,
            height: am5.p100
        }));

// Set up formats
//        root.numberFormatter.setAll({
//            numberFormat: "#.##as"
//        });


// ===========================================================
// XY chart
// ===========================================================

// Create chart
        var chart = container.children.push(am5xy.XYChart.new(root, {
            panX: false,
            panY: false,
            wheelX: "none",
            wheelY: "none",
            layout: root.verticalLayout,
            width: am5.percent(100)
        }));

// Create axes
        yAxis1 = chart.yAxes.push(am5xy.CategoryAxis.new(root, {
            categoryField: "age",
            renderer: am5xy.AxisRendererY.new(root, {
                minorGridEnabled: true,
                minGridDistance: 15
            })
        }));
        yAxis1.get("renderer").grid.template.set("location", 1);
        yAxis1.get("renderer").labels.template.set("fontSize", 11);




//        var xAxis = chart.xAxes.push(am5xy.ValueAxis.new(root, {
//            min: -10,
//            max: 10,
//            extraMax: 5,
//            numberFormat: "#.s'%'",
//            renderer: am5xy.AxisRendererX.new(root, {
//                minGridDistance: 20
//            })
//        }));

var xAxis = chart.xAxes.push(
  am5xy.ValueAxis.new(root, {

    renderer: am5xy.AxisRendererX.new(root, {
      minGridDistance: 20
    })
  })
);


        xAxis.get("renderer").labels.template.set("fontSize", 11);

// Create series
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


// Add labels
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
        //fill: am5.color(0xffffff),
        
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



    }); // end am5.ready()    

    
    return([maleSeries, femaleSeries, yAxis1]);
}



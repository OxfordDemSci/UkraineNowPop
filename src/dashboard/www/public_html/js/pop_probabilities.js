import * as _utils from './utils.js?version=0.46'


export function calc_pop_probabilities(d) {

    var data = d.sort();
    var binNum = 100;
    var mean = jStat.mean(data);
    var stdev = jStat.stdev(data);

    var min = jStat.min(data);
    var max = jStat.max(data);

    var normData = jStat.seq(min, max, binNum, (x) => {
        return jStat.normal.pdf(x, mean, stdev);
    });

    var normSample = jStat.seq(min, max, binNum, (x) => {
        return Math.round(x);
    });
    
   
//    var normSample = jStat.seq(min, max, binNum, (x) => {
//        return Math.round(jStat.normal.sample(min, stdev));
//    });    

    return [
        normData.map(function (x) {
            return (x * data.length * binNum).toFixed(4);
        }),
        normSample
    ];

}

const round = (n, dp) => {
  const h = +('1'.padEnd(dp + 1, '0')); // 10 or 100 or 1000 or etc
  return Math.round(n * h) / h;
};

function convert(d){
    var x = [];
    var y = [];
    // set properly optimise plotting hundreds/thousands 1000 every 10
    // density multiply by 1000
    for(let i=0; i<d.length; i+=1){
        y.push( round(d[i].x, 3));
        x.push( round(d[i].y*10000, 6));
    }
    return [x, y];
}


export function update_pop_probabilities(d) {
    
    //let res_probabilities = calc_pop_probabilities(d);
    //console.log(res_probabilities);
    let res_probabilities=convert(d);
    let dataX=res_probabilities[0];
    let dataY=res_probabilities[1];
    
    var option;

    option = {
        grid: {
            left: '26%',
            bottom: '20%',
            top: '13%'
        },
        tooltip: {
            trigger: 'axis',
            formatter: 'Population: {b}'
        },
        xAxis: {
            type: 'category',
            boundaryGap: true,
            data: dataY,
            name: 'Population',
            nameLocation: 'center',
            nameTextStyle: {
                align: 'center',
                verticalAlign: 'top',
                /**
                 * the top padding will shift the name down so that it does not overlap with the axis-labels
                 * t-l-b-r
                 */
                padding: [20, 0, 0, 0]
            },
            axisLabel: {
                interval: 20,
                showMinLabel: true,
                showMaxLabel: true,
                rotate: 30,
                fontSize: 10,
                formatter: val => _utils.nFormatter(val, 2)
            },
            axisTick: {
                interval:20
            }            
        },
        yAxis: {
            type: 'value',
            name: 'Probability',
            nameLocation: 'center',
            nameTextStyle: {
                align: 'center',
                verticalAlign: 'top',
                /**
                 * the top padding will shift the name down so that it does not overlap with the axis-labels
                 * t-l-b-r
                 */
                padding: [-45, 0, 0, 0]
            }
        },
        series: [
            {
                data: dataX,
                type: 'line',
                areaStyle: {},
                large: true,
                largeThreshold: 10
            }
        ]
    };

    //var chartDom = document.getElementById('plotProbability');
    var myChart = echarts.init(chartDom);
    option && myChart.setOption(option, true);

}
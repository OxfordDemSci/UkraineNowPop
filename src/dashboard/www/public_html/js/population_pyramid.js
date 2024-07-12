
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

function aggregateData(list) {
    var maleTotal = 0;
    var femaleTotal = 0;

    for (var i = 0; i < list.length; i++) {
        var row = list[i];
        maleTotal += row.male;
        femaleTotal += row.female;
    }

    for (var i = 0; i < list.length; i++) {
        var row = list[i];
        row.malePercent = -1 * Math.round((row.male / maleTotal) * 10000) / 100;
        row.femalePercent = Math.round((row.female / femaleTotal) * 10000) / 100;
    }

    return list;
}

function getRandomInt(max) {
    return Math.floor(Math.random() * max);
}

export function updatePopulationPyramid(data, series, initial_age_ranges) {


    var values_pyramid = [];
    
    for (var i = 0; i < initial_age_ranges.length; i++) {
        
        let age_min = initial_age_ranges[i]["age_min"];
        let age_max = initial_age_ranges[i]["age_max"];
        
        let female_population_entry = data.female_population
                .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
                .map(entry => entry.population);   
        
        let male_population_entry = data.male_population
                .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
               .map(entry => entry.population);        

        
        values_pyramid.push(
                {
                    age: age_min + "-" + age_max,
                    male: (male_population_entry.length > 0 ) ? male_population_entry[0] : null,
                    female: (female_population_entry.length > 0 ) ? female_population_entry[0] : null
                }
        );   

 

    }       
    
 
//    for (var i = 0; i < data.female_population.length; i++) {
//        
//        let age_min = data.female_population[i]["age_min"];
//        let age_max = data.female_population[i]["age_max"];
//        
//        let female_population_entry = data.female_population[i].population; 
//        
//        let male_population_entry = data.male_population[i].population; 
//        
//        values_pyramid.push(
//                {
//                    age: age_min + "-" + age_max,
//                    male: male_population_entry,
//                    female: female_population_entry
//                }
//        );        
//
//    }    



//    for (var i = 0; i < initial_age_ranges.length; i++) {
//        
//        let age_min = initial_age_ranges[i]["age_min"];
//        let age_max = initial_age_ranges[i]["age_max"];
//
//        let female_population_entry = data.female_population
//                .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
//                .map(entry => entry.population);
//
//
//        let male_population_entry = data.male_population
//                .filter(entry => (entry.age_min === age_min && entry.age_max === age_max))
//                .map(entry => entry.population);
//
////        values_pyramid.push(
////                {
////                    age: age_min + "-" + age_max,
////                    male: male_population_entry[0] - getRandomInt(male_population_entry[0]),
////                    female: female_population_entry[0] - getRandomInt(female_population_entry[0])
////                }
////        );
//
//        values_pyramid.push(
//                {
//                    age: age_min + "-" + age_max,
//                    male: male_population_entry[0],
//                    female: female_population_entry[0]
//                }
//        );
//    }

        let SelectedSex=$('#idSelectSex option').filter(":selected").val();
 
        if (SelectedSex === "Female"){
                for (var i = 0; i < values_pyramid.length; i++){
                    var obj = values_pyramid[i];
                    for (var key in obj){
                        if (key === "male"){
                            values_pyramid[i][key] = null;
                        }
                    }
                }
        }
        
        if (SelectedSex === "Male"){
                for (var i = 0; i < values_pyramid.length; i++){
                    var obj = values_pyramid[i];
                    for (var key in obj){
                        if (key === "female"){
                            values_pyramid[i][key] = null;
                        }
                    }
                }
        }        
 

    values_pyramid = aggregateData(values_pyramid);

    series[2].data.setAll(values_pyramid);
    series[0].data.setAll(values_pyramid);
    series[1].data.setAll(values_pyramid);

}
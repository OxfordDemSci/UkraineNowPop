import * as _utils from './utils.js?version=0.75'

export function download_csv(api_url,
        country,
        admin_level,
        age_ranges_available,
        accessToken) {

    let age_min_male = age_ranges_available[0][$(".slider_age_selections").slider("values", 0)];
    let age_max_male = age_ranges_available[1][$(".slider_age_selections").slider("values", 1)];

    let age_min_female = age_ranges_available[0][$(".slider_age_selections").slider("values", 0)];
    let age_max_female = age_ranges_available[1][$(".slider_age_selections").slider("values", 1)];

    let age_min = age_min_male;
    let age_max = age_max_male;

    let SelectedSex = $('#idSelectSex option').filter(":selected").val();

    if (SelectedSex === "Female") {
        age_min_male = 0;
        age_max_male = 0;
        age_min = age_min_female;
        age_max = age_max_female;
    }
    if (SelectedSex === "Male") {
        age_min_female = 0;
        age_max_female = 0;
        age_min = age_min_male;
        age_max = age_max_male;
    }

    let date_start = $("#datepickerStartDate").data('datepicker').getFormattedDate('yyyy-mm-dd');
    let date_end = $("#datepickerEndDate").data('datepicker').getFormattedDate('yyyy-mm-dd');


    let uri = api_url + "/data_download?country=" + country + "&admin_level=" + admin_level + "&date_start=" + date_start + "&date_end=" + date_end + "&age_min=" + age_min + "&age_max=" + age_max + "&sex=" + SelectedSex.toLowerCase() + "&file_format=csv";

    var to_date = new Date(date_end);
    var from_date = new Date(date_start);
    var delta = to_date - from_date;
    if(delta < 0){  
        console.log("End data should be more then start date");
        return;
    };

    let fname_downloaded = country + "_" + date_start + "_" + date_end + "_" + age_min + "_" + age_max + "_" + SelectedSex.toLowerCase() + ".csv";

    _utils.progressMenuOn();

    fetch(uri)
            .then(resp => resp.status === 200 ? resp.blob() : Promise.reject('something went wrong'))
            .then(blob => {
                const url = window.URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.style.display = 'none';
                a.href = url;
                // the filename you want
                a.download = fname_downloaded;
                document.body.appendChild(a);
                a.click();
                window.URL.revokeObjectURL(url);
                $('#idMdDownload').modal('hide');
                _utils.progressMenuOff();
            })
            .catch(() => {
                _utils.progressMenuOff();
                alert('oh no!');
            });


}
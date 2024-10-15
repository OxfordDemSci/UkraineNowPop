import pyidp
import os
from math import ceil
import pandas as pd
from plotly import subplots
import plotly.graph_objects as go




#---- time series plot ----#

def timeseries_data(args, proportional=True):
    df_list = {}
    for k in args.keys():
        # k = list(args.keys())[0]

        args_item = args.get(k)

        # create data
        args_item['platform'] = 'facebook'
        df_fb = pyidp.query_api(endpoint='query_clean', args=args_item)
        df_fb.drop_duplicates(inplace=True,
                              subset=['collection_date', 'country', 'geo_key', 'age_min', 'age_max', 'gender', 'location_types', 'language_name'])
        df_fb['platform'] = 'facebook'

        args_item['platform'] = 'instagram'
        df_ig = pyidp.query_api(endpoint='query_clean', args=args_item)
        df_ig.drop_duplicates(inplace=True,
                              subset=['collection_date', 'country', 'geo_key', 'age_min', 'age_max', 'gender', 'location_types', 'language_name'])
        df_ig['platform'] = 'instagram'

        # select gaza municipalities
        if args_item['collection_name'] == 'gaza_municipalities':
            df_fb = pyidp.select_gaza_municipalities(df_select=df_fb)
            df_ig = pyidp.select_gaza_municipalities(df_select=df_ig)

        # calculate relative audience
        if proportional:
            df_fb = pyidp.relative_audience(df_fb)
            df_ig = pyidp.relative_audience(df_ig)

        # merge data for both platforms
        df = pd.concat([df_fb, df_ig])

        # sort
        df = df.sort_values(by=['geo_name', 'platform', 'collection_date'])

        cols_keep = ['collection_date', 'geo_name', 'platform', 'dau_relative', 'mau_lower_relative', 'mau_upper_relative']
        if not proportional:
            cols_keep = [x.replace('_relative', '') for x in cols_keep]
        df = df[cols_keep]

        # long format audience
        value_vars = ['dau', 'mau_lower']
        if proportional:
            value_vars = [x + '_relative' for x in value_vars]

        df = pd.melt(df,
                     id_vars=['collection_date', 'geo_name', 'platform'],
                     value_vars=value_vars)

        df_list[k] = df

    return df_list


def timeseries_plot(df_dict, title, outdir=None):

    # demographic groups
    demographics = list(df_dict.keys())

    # colors
    geo_names = df_dict[demographics[0]]['geo_name'].unique()
    colors = dict(zip(geo_names,
                      pyidp.get_colors(num_colors=len(list(geo_names)))))

    # initiate visibility vectors
    visibility = dict()
    for i in demographics:
        visibility[i] = []

    # initiate panel data
    platforms = ['facebook', 'instagram']
    variables = list(df_dict[list(df_dict.keys())[0]]['variable'].unique())

    data_panels = {}
    for variable in variables:
        for platform in platforms:
            panel_name = '_'.join([platform, variable])
            data_panels[panel_name] = []

    # traces: scatter plots
    for platform in platforms:
        for variable in variables:
            panel = '_'.join([platform, variable])
            for demographic in demographics:

                # data and subsets
                df_dem = df_dict[demographic]
                df_dem_panel = df_dem.loc[df_dem['platform'].eq(platform) & df_dem['variable'].eq(variable),]
                df_dem_panel_by_geoname = df_dem_panel.groupby(by='geo_name')

                for group, df_group in df_dem_panel_by_geoname:

                    # save trace
                    trace = go.Scatter(name=group,
                                       x=df_group.collection_date.tolist(),
                                       y=df_group.value.tolist(),
                                       mode='lines+markers',
                                       marker=dict(color=colors[group]),
                                       line=dict(color=colors[group]),
                                       visible=(demographic == demographics[0]),
                                       legendgroup=group,
                                       showlegend=(panel == list(data_panels.keys())[0]),
                                       hovertemplate='<b>Location:</b> ' + group + '</br><b>Date:</b> %{x}<br><b>Audience:</b> %{y:.3f}'
                                       )

                    # append trace to panel data
                    data_panels[panel].append(trace)

                    # update visibility for this trace for each demographic button
                    visibility[demographic].append(True)
                    for i in [x for x in demographics if x != demographic]:
                        visibility[i].append(False)

    # drop-down buttons
    buttons = list()
    for demographic in demographics:
        buttons.append(
            dict(
                label=demographic.replace('_', ' ').title(),
                method='update',
                args=[{'visible': visibility[demographic]}]
            )
        )

    updatemenus = list([
        dict(buttons=buttons)
    ])

    # initiate figure with four panels
    row_titles = [x.replace('_relative', '').
                  replace('dau', 'Daily Active Users').
                  replace('mau_lower', 'Monthly Active Users').
                  replace('mau_upper', 'Monthly Active Users')
                  for x in variables]

    fig = subplots.make_subplots(rows=ceil(len(data_panels) / 2),
                                 cols=2,
                                 row_titles=row_titles,
                                 column_titles=[x.title() for x in platforms])

    # add traces
    row = 1
    col = 1
    for panel in data_panels.keys():
        fig.update_xaxes(title_text='Date', row=row, col=col)

        if '_relative' in variables[0]:
            fig.update_yaxes(title_text='Proportion of users', row=row, col=col)
        else:
            fig.update_yaxes(title_text='Count of users', row=row, col=col)

        for i in range(len(data_panels[panel])):
            fig.add_trace(data_panels[panel][i], row=row, col=col)

        if col == 1:
            col += 1
        else:
            row += 1
            col = 1

    # plot layout
    fig.update_layout(title=title.replace('_', ' ').title(),
                      updatemenus=updatemenus)

    if outdir is None:
        # plot to browser
        fig.show()
    else:
        # save to html
        os.makedirs(outdir, exist_ok=True)
        fig.write_html(os.path.join(outdir, title + '.html'))


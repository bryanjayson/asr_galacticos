import soccerdata as sd
import pandas as pd
import json
import numpy as np

# defining paths for config and data storage
storage_path = 'C:\\Users\\Bryan\\SynologyDrive\\Universität\\Master\\24 FS\\Applied Sports Research\\Data'
scraper_config_path = 'C:\\Users\\Bryan\\soccerdata\\config'

# defining timeframes and leagues
seasons = ['2022/2023', '2021/2022', '2020/2021', '2019/2020', '2018/2019', '2017/2018', '2016/2017', '2015/2016',
           '2014/2015', '2013/2014', '2012/2013', '2011/2012', '2010/2011', '2009/2010']

dates = ['2023-05-31',   '2022-05-31', '2021-05-31', '2020-05-31', '2019-05-31', '2018-05-31', '2017-05-31',
         '2016-05-31', '2015-05-31', '2014-05-31', '2013-05-31', '2012-05-31', '2011-05-31']

leagues = ['ENG-Premier League', 'ESP-La Liga', 'FRA-Ligue 1', 'GER-Bundesliga', 'ITA-Serie A']

leagues_dict = {
    'Premier League': 'ENG-Premier League',
    'LaLiga': 'ESP-La Liga',
    'Ligue 1': 'FRA-Ligue 1',
    'Bundesliga': 'GER-Bundesliga',
    'Serie A': 'ITA-Serie A'}

# defining common team names across all datasets (different sources use different naming conventions)
clubname_mapping = pd.read_excel(f"{storage_path}\\python\\clubname_mapping.xlsx", header=None)
clubname_dict = {}

for index, row in clubname_mapping.iterrows():
    key = row[0]
    values = [value for value in row[1:] if pd.notna(value)]
    clubname_dict[key] = values

clubname_json = json.dumps(clubname_dict, ensure_ascii=False, indent=4)
with open(f"{storage_path}\\python\\clubname_mapping.json", 'w', encoding='utf-8') as json_file:
    json_file.write(clubname_json)
with open(f"{scraper_config_path}\\teamname_replacements.json", 'w', encoding='utf-8') as json_file_2:
    json_file_2.write(clubname_json)

# adjust season formats from "1011" to "2010/2011
def adjust_season_format(season):
    season_str = str(season)
    adjusted_season = "20" + season_str[:2] + "/" + "20" + season_str[2:]
    return adjusted_season

# simplify workflow
rankings = 0
elo = 0
injuries = 0
transfers = 1
marketvalue = 0
fifa_ratings = 0
join_all = 1

if rankings == 1:
    fotmob = sd.FotMob(leagues=leagues, seasons=seasons)
    league_tables = fotmob.read_league_table()

    league_tables.reset_index(inplace=True)
    league_tables['season'] = league_tables['season'].apply(adjust_season_format)
    league_tables['PPG'] = league_tables['Pts'] / league_tables['MP']
    league_tables['win_pct'] = league_tables['W'] / league_tables['MP']

    # standardize club_names
    inverted_clubname_mapping = {v: k for k, lst in clubname_dict.items() for v in lst}
    league_tables['team'] = league_tables['team'].apply(lambda name:inverted_clubname_mapping.get(name, name))

    def find_promoted_teams(df):
        df['season_start_year'] = df['season'].apply(lambda x: int(x.split('/')[0]))
        df.sort_values(by=['league', 'season_start_year'], inplace=True)

        promoted = []
        for league, league_df in df.groupby('league'):
            seasons = sorted(league_df['season_start_year'].unique())
            teams_by_season = {season: set(league_df[league_df['season_start_year'] == season]['team']) for season in seasons}

            for season in seasons:
                previous_season = season - 1
                if previous_season in teams_by_season:
                    current_teams = teams_by_season[season]
                    previous_teams = teams_by_season[previous_season]
                    promoted_teams = current_teams - previous_teams
                    promoted += [(1 if team in promoted_teams else 0) for team in league_df[league_df['season_start_year'] == season]['team']]
                else:
                    promoted += [0] * len(league_df[league_df['season_start_year'] == season])

        df['promoted'] = promoted
        return df.drop(columns=['season_start_year'])

    league_tables['rank'] = league_tables.sort_values(by=['Pts', 'GD', 'GF'], ascending=[False, False, False]).groupby(['league', 'season']).cumcount() + 1
    league_tables['last_rank'] = league_tables.groupby(['league', 'season'])['rank'].transform(max)
    league_tables['LOR'] = np.log((league_tables['last_rank'] + 1 - league_tables['rank']) / league_tables['rank'])

    league_tables = find_promoted_teams(league_tables)

    league_tables.to_excel(f"{storage_path}\\engineered\\historical_rankings.xlsx", index=False)

# extracting historical club elo ratings
if elo == 1:
    clubelo = sd.ClubElo()
    df_elos = []
    date_to_season_dict = dict(zip(dates, seasons))

    for date in dates:
        df_elo_bydate = clubelo.read_by_date(date=date)
        df_elo_bydate['extract_date'] = date
        df_elo_bydate['season'] = df_elo_bydate['extract_date'].map(date_to_season_dict)
        df_elo_bydate.reset_index(inplace=True)
        df_elos.append(df_elo_bydate)

    df_historical_elos = pd.concat(df_elos, ignore_index=True)
    df_historical_elos = df_historical_elos[df_historical_elos['league'].isin(leagues)]

    df_historical_elos.sort_values(by=['team', 'season'], inplace=True)
    df_historical_elos['elo_season_delta'] = df_historical_elos.groupby('team')['elo'].diff()

    # dropping unused columns
    df_historical_elos['elo_season_rank'] = df_historical_elos['rank']
    df_historical_elos.drop(columns=['to', 'from', 'country', 'level', 'extract_date', 'rank'], inplace=True)

    # standardize club_names
    inverted_clubname_mapping = {v: k for k, lst in clubname_dict.items() for v in lst}
    df_historical_elos['team'] = df_historical_elos['team'].apply(lambda name: inverted_clubname_mapping.get(name, name))

    df_historical_elos.to_excel(f"{storage_path}\\engineered\\historical_elos.xlsx", index=False)

# extracting injured players list
if injuries == 1:
    extract = 0

    if extract == 1:
        seasons_subset = ['2022/2023', '2021/2022', '2020/2021', '2019/2020', '2018/2019']
        seasons_subset_2 = ['2011/2012', '2010/2011', '2009/2010']
        leagues_subset_1 = ['GER-Bundesliga', 'ITA-Serie A', 'ESP-La Liga']
        leagues_subset_2 = ['ENG-Premier League', 'FRA-Ligue 1']

        whoscored = sd.WhoScored(leagues=leagues, seasons=seasons_subset_2)
        missing_players = whoscored.read_missing_players(force_cache=True)

        missing_players.reset_index(inplace=True)
        missing_players.to_excel(f"{storage_path}\\raw\\injuries_all_12-10.xlsx")

    # cleaning injury data
    injury_df1 = pd.read_excel(f"{storage_path}\\raw\\injuries_DE_GER_ES.xlsx")
    injury_df2 = pd.read_excel(f"{storage_path}\\raw\\injuries_ENG_FR.xlsx")
    injury_df3 = pd.read_excel(f"{storage_path}\\raw\\injuries_all_17-15.xlsx")
    injury_df4 = pd.read_excel(f"{storage_path}\\raw\\injuries_all_14-12.xlsx")
    injury_df5 = pd.read_excel(f"{storage_path}\\raw\\injuries_all_12-10.xlsx")

    # combining and dropping unused column
    injury_df_all = pd.concat([injury_df1, injury_df2, injury_df3, injury_df4, injury_df5], ignore_index=True)
    injury_df_all.drop(columns=injury_df_all.columns[0], inplace=True)

    aggregated_injuries = injury_df_all.groupby(['league', 'season', 'game_id', 'team', 'status']).size().unstack(fill_value=0).reset_index()
    aggregated_injuries.columns = ['league', 'season', 'game_id', 'team', 'doubtful', 'out']

    # standardize club_names
    inverted_clubname_mapping = {v: k for k, lst in clubname_dict.items() for v in lst}
    aggregated_injuries['team'] = aggregated_injuries['team'].apply(lambda name:inverted_clubname_mapping.get(name, name))

    # reformat season identifier
    aggregated_injuries['season'] = aggregated_injuries['season'].apply(adjust_season_format)
    aggregated_injuries.to_excel(f"{storage_path}\\raw\\aggregated_injuries.xlsx", index=False)

    # average injuries over each season
    average_injuries = aggregated_injuries.groupby(['league', 'season', 'team'])[['doubtful', 'out']].mean().reset_index()
    average_injuries.to_excel(f"{storage_path}\\engineered\\average_injuries.xlsx", index=False)

if transfers == 1:
    csv_names = {'bundesliga.csv': 'GER-Bundesliga',
                 'ligue-1.csv': 'FRA-Ligue 1',
                 'premier-league.csv': 'ENG-Premier League',
                 'primera-division.csv': 'ESP-La Liga',
                 'serie-a.csv': 'ITA-Serie A'}

    df_transfers = []

    for league_csv, league_id in csv_names.items():
        raw_transfers = pd.read_csv(f"{storage_path}\\raw\\transferdata\\{league_csv}")
        raw_transfers_filtered = raw_transfers[raw_transfers['season'].isin(seasons)]

        fee_thresholds = raw_transfers_filtered.groupby(['season'])['fee_cleaned'].quantile(0.95).reset_index(name='fee_threshold_95th')
        fee_thresholds['league'] = league_id
        raw_transfers_filtered = raw_transfers_filtered.merge(fee_thresholds, on=['season'])
        raw_transfers_filtered['above_95th_percentile'] = np.where(
            (raw_transfers_filtered['transfer_movement'] == 'in'), raw_transfers_filtered['fee_cleaned'] > raw_transfers_filtered['fee_threshold_95th'], False)

        net_transfers = raw_transfers_filtered.groupby(['club_name', 'season']).apply(
            lambda x: pd.Series({
                'gross_incoming': x[x['transfer_movement'] == 'in']['fee_cleaned'].sum(),
                'gross_outgoing': x[x['transfer_movement'] == 'out']['fee_cleaned'].sum(),
                'arrivals': x[x['transfer_movement'] == 'in']['fee_cleaned'].count(),
                'departures': x[x['transfer_movement'] == 'out']['fee_cleaned'].count(),
                'high_value_transfers': 1 if x['above_95th_percentile'].sum() > 0 else 0,
                'fee_threshold_95th': x['fee_threshold_95th'].iloc[0]})).reset_index()

        net_transfers['net_spend'] = net_transfers['gross_incoming'] - net_transfers['gross_outgoing']
        net_transfers['league'] = league_id

        total_spending_per_season = net_transfers.groupby('season')['net_spend'].sum().reset_index(name='total_net_spend_league')
        average_spending_per_season = net_transfers.groupby('season')['net_spend'].mean().reset_index(name='avg_net_spend_league')
        average_gross_spend_season = net_transfers.groupby('season')['gross_incoming'].mean().reset_index(name='avg_gross_league')

        net_transfers = pd.merge(net_transfers, total_spending_per_season, on='season', how='left')
        net_transfers = pd.merge(net_transfers, average_spending_per_season, on='season', how='left')
        net_transfers = pd.merge(net_transfers, average_gross_spend_season, on='season', how='left')

        net_transfers['relative_total_spend'] = net_transfers['net_spend'] / net_transfers['total_net_spend_league']
        net_transfers['relative_avg_net_spend'] = net_transfers['net_spend'] / net_transfers['avg_net_spend_league']
        net_transfers['relative_avg_gross_spend'] = net_transfers['gross_incoming'] / net_transfers['avg_gross_league']

        net_transfers['net_spend_3Y'] = net_transfers.groupby('club_name')['net_spend'].rolling(window=3, min_periods=3).sum().reset_index(0, drop=True)
        net_transfers['net_spend_2Y'] = net_transfers.groupby('club_name')['net_spend'].rolling(window=2, min_periods=2).sum().reset_index(0, drop=True)

        df_transfers.append(net_transfers)

    aggregated_transfers = pd.concat(df_transfers, ignore_index=True)

    inverted_clubname_mapping = {v: k for k, lst in clubname_dict.items() for v in lst}
    aggregated_transfers['club_name'] = aggregated_transfers['club_name'].apply(lambda name: inverted_clubname_mapping.get(name, name))
    aggregated_transfers.rename(columns={'club_name': 'team'}, inplace=True)

    aggregated_transfers.to_excel(f"{storage_path}\\engineered\\aggregated_transfers.xlsx", index=False)

# aggregating raw player values to overall team values per club
if marketvalue == 1:
    player_values = pd.read_csv(f"{storage_path}\\raw\\player_values.csv")
    player_values['player_market_value_euro'] = player_values['player_market_value_euro'] / 1000000

    team_values = player_values.groupby(['comp_name', 'season_start_year', 'squad']).agg(team_value=('player_market_value_euro', 'sum'),
                                                                                         avg_squad_age=('player_age', 'mean')).reset_index()

    # Adjusting season identifiers, club names, column names
    team_values['season'] = team_values['season_start_year'].apply(lambda x: f"{x}/{x + 1}")
    inverted_clubname_mapping = {v: k for k, lst in clubname_dict.items() for v in lst}
    team_values['team'] = team_values['squad'].apply(lambda name: inverted_clubname_mapping.get(name, name))

    leagues_dict = {'Premier League': 'ENG-Premier League',
                    'LaLiga': 'ESP-La Liga',
                    'Ligue 1': 'FRA-Ligue 1',
                    'Bundesliga': 'GER-Bundesliga',
                    'Serie A': 'ITA-Serie A'}

    team_values['league'] = team_values['comp_name'].map(leagues_dict)

    # adding last year team_value
    team_values.sort_values(by=['team', 'season_start_year'], inplace=True)
    team_values['lagged_team_value'] = team_values.groupby('team')['team_value'].shift(1)

    team_values.drop(columns=['comp_name', 'season_start_year', 'squad'], inplace=True)
    team_values.to_excel(f"{storage_path}\\engineered\\team_values.xlsx", index=False)

if fifa_ratings == 1:
    # note: not used in final paper
    sofifa = sd.SoFIFA(leagues=leagues, versions=[230001, 220001, 210001, 200001, 190001, 180001, 170001, 160001, 150001, 140001, 130001, 120001])

    df_team_ratings_raw = sofifa.read_team_ratings()
    df_team_ratings_raw.reset_index(inplace=True)

    # convert game year to season identifier
    def convert_to_season(game_year):
        numeric_year = int(''.join(filter(str.isdigit, game_year)))
        calendar_year_start = 2000 + numeric_year - 1
        calendar_year_end = 2000 + numeric_year
        return f"{calendar_year_start}/{calendar_year_end}"

    #standardize team names
    inverted_clubname_mapping = {v: k for k, lst in clubname_dict.items() for v in lst}
    df_team_ratings_raw['team'] = df_team_ratings_raw['team'].apply(lambda name: inverted_clubname_mapping.get(name, name))

    df_team_ratings_raw['season'] = df_team_ratings_raw['fifa_edition'].apply(convert_to_season)
    df_team_ratings = df_team_ratings_raw[['league', 'season', 'team', 'overall', 'international_prestige']]

    df_team_ratings_raw.to_excel(f"{storage_path}\\raw\\fifa_team_rankings_raw.xlsx", index=False)
    df_team_ratings.to_excel(f"{storage_path}\\engineered\\fifa_team_ratings.xlsx", index=False)

# putting everything together
if join_all == 1:
    df_transfers = pd.read_excel(f"{storage_path}\\engineered\\aggregated_transfers.xlsx")
    df_elos = pd.read_excel(f"{storage_path}\\engineered\\historical_elos.xlsx")
    df_rankings = pd.read_excel(f"{storage_path}\\engineered\\historical_rankings.xlsx")
    df_values = pd.read_excel(f"{storage_path}\\engineered\\team_values.xlsx")
    df_injuries = pd.read_excel(f"{storage_path}\\engineered\\average_injuries.xlsx")
    df_coefficients = pd.read_excel(f"{storage_path}\\engineered\\uefa_club_coefficients.xlsx")
    df_cups = pd.read_excel(f"{storage_path}\\engineered\\cup_winners.xlsx")
    df_fifa = pd.read_excel(f"{storage_path}\\engineered\\fifa_team_ratings.xlsx")

    inverted_clubname_mapping = {v: k for k, lst in clubname_dict.items() for v in lst}
    df_coefficients['team'] = df_coefficients['team'].apply(lambda name: inverted_clubname_mapping.get(name, name))
    df_cups['team'] = df_cups['team'].apply(lambda name: inverted_clubname_mapping.get(name, name))

    merged_df = df_elos.merge(df_rankings, on=['league', 'season', 'team'], how='outer')
    merged_df = merged_df.merge(df_values, on=['league', 'season', 'team'], how='outer')
    merged_df = merged_df.merge(df_transfers, on=['league', 'season', 'team'], how='outer')
    merged_df = merged_df.merge(df_injuries, on=['league', 'season', 'team'], how='outer')
    merged_df = merged_df.merge(df_fifa, on=['league', 'season', 'team'], how='outer')
    merged_df = merged_df.merge(df_coefficients, on=['league', 'season', 'team'], how='left')
    merged_df = merged_df.merge(df_cups, on=['league', 'season', 'team'], how='left')
    merged_df['cup_won'] = merged_df['cup_won'].fillna(0)

    # normalize club names
    inverted_clubname_mapping = {v: k for k, lst in clubname_dict.items() for v in lst}
    merged_df['team'] = merged_df['team'].map(lambda name: inverted_clubname_mapping.get(name, name))

    # just to be sure
    leagues_dict = {'Premier League': 'ENG-Premier League',
                    'LaLiga': 'ESP-La Liga',
                    'Ligue 1': 'FRA-Ligue 1',
                    'Bundesliga': 'GER-Bundesliga',
                    'Serie A': 'ITA-Serie A'}

    inverted_league_mapping = {v: k for k, v in leagues_dict.items()}
    merged_df['league'] = merged_df['league'].map(inverted_league_mapping)

    # dropping unused incomplete data
    merged_df = merged_df[~merged_df['season'].isin(["2009/2010", "2091/200", '2010/2011'])]

    columns_to_normalize = ['team_value', 'net_spend', 'lagged_team_value']

    def z_normalize(group):
        columns = columns_to_normalize
        means = group[columns].mean()
        std_devs = group[columns].std()
        # Normalize only the specified columns
        normalized = (group[columns] - means) / std_devs
        # Attach 'team', 'league', 'season' to ensure correct mapping
        normalized['team'] = group['team']
        normalized['league'] = group['league']
        normalized['season'] = group['season']
        return normalized

    normalized_df = merged_df.groupby(['league', 'season']).apply(z_normalize).reset_index(drop=True)
    normalized_columns = {col: f'league_normalized_{col}' for col in columns_to_normalize}
    normalized_df.rename(columns=normalized_columns, inplace=True)

    # merge the normalized data back into the original DataFrame
    final_df = pd.merge(merged_df, normalized_df, on=['team', 'league', 'season'])

    final_df.to_excel(f"{storage_path}\\engineered\\galacticos_dataset.xlsx", index=False)

print()
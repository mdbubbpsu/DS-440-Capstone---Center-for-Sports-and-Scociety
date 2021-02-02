from os import name
import requests
import json
import pandas as pd


def createMasterDf():
    bigTen = ['Ohio State', 'Indiana', 'Penn State', 'Maryland', 'Rutgers', 'Michigan', 'Michigan State',
              'Northwestern', 'Iowa', 'Wisconsin', 'Minnesota', 'Nebraska', 'Purdue', 'Illinois']
    masterDf = pd.DataFrame()
    for team in bigTen:
        print(team)
        for year in range(2010, 2020):
            if ' ' in team:
                reqString = 'https://api.collegefootballdata.com/games?year=' + \
                    str(year) + '&seasonType=regular&team=' + \
                    team[:team.index(' ')]+'%20' + team[team.index(' ')+1:]
            else:
                reqString = 'https://api.collegefootballdata.com/games?year=' + \
                    str(year) + '&seasonType=regular&team=' + team
            response = requests.get(reqString)
            jsonObj = response.json()
            masterDf = masterDf.append(pd.read_json(json.dumps(jsonObj)))
    return masterDf


if __name__ == '__main__':
    df = createMasterDf()
    print(df)
    writer = pd.ExcelWriter('test.xlsx', engine='xlsxwriter')
    df.to_excel(writer, header=True)
    writer.save()

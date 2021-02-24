import pandas as pd
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import sys
from numpy.polynomial.polynomial import polyfit

# Change directory to your root folder
os.chdir("/Users/mattbubb/Spring_2021/DS_440/DS-440-Capstone---Center-for-Sports-and-Scociety")
t = pd.read_csv('./Data/CFBeattendanceCSV.csv')


def yearVsAtt(teams):
    for team in teams:
        a = t.loc[t['Team'] == team]
        col = []
        for i in range(a.index[0], a.index[0] + len(a)):
            if a.loc[i]['Opponent_Rank'] == 'NR':
                col.append('blue')
            else:
                col.append('red')

        # x = sorted(a['Time'])
        x = sorted(a['Year'])
        y = list(a['Attendance'])
        xAry = np.array(x)
        yAry = np.array(y)
        m, b = np.polyfit(xAry, yAry, 1)
        # print(m, b)
        plt.xlabel('Year')
        plt.ylabel('In person Attendence')
        plt.figure(figsize=(6, 7))
        scatter = plt.scatter(x, y, c=col)
        plt.plot(xAry, m*xAry + b)
        titleString = 'Year vs. In-Person attendance at', team, 'Football Games'
        plt.title(titleString)
        redDots = mpatches.Patch(color='red', label='Opponent Ranked')
        blueDots = mpatches.Patch(color='blue', label='Opponent Not Ranked')
        plt.legend(handles=[redDots, blueDots])
    plt.show()


def currentWinsVsAtt(teams):
    for team in teams:
        a = t.loc[t['Team'] == team]
        col = []
        colorDict = {}
        currentYear = 2000
        currentColor = 'red'
        for i in range(a.index[0], a.index[0] + len(a)):
            if a.loc[i]['Year'] == currentYear:
                colorDict[currentYear] = currentColor
                col.append(currentColor)
            else:
                currentYear = a.loc[i]['Year']
                currentColor = np.random.rand(3,)
                colorDict[currentYear] = currentColor
                col.append(currentColor)

        # x = sorted(a['Time'])
        x = a['Current Wins']
        y = list(a['Attendance'])
        xAry = np.array(x)
        yAry = np.array(y)

        m, b = np.polyfit(xAry, yAry, 1)
        plt.xlabel('Current Wins')
        plt.ylabel('Attendance')
        plt.figure(figsize=(8, 6))
        scatter = plt.scatter(x, y, c=col)
        plt.plot(xAry, m*xAry + b)
        titleString = 'Current Amount of wins vs. In-Person attendance at', team, 'Football Games'
        plt.title(titleString)
        lst = [mpatches.Patch(color=v, label=k) for k, v in colorDict.items()]
        plt.legend(handles=lst)

    plt.show()


if __name__ == '__main__':
    l = ['Penn State', 'Illinois', 'Rutgers', 'Wisconsin',
         'Michigan State', 'Northwestern', 'Indiana', 'Nebraska']
    yearVsAtt(l)
    # currentWinsVsAtt(l)

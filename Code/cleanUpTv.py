import pandas as pd
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

from numpy.polynomial.polynomial import polyfit
from sklearn.linear_model import LinearRegression
os.chdir('/Users/mattbubb/Spring_2021/DS_440/DS-440-Capstone---Center-for-Sports-and-Scociety')
t = pd.read_csv('Data/CFBeattendanceCSV.csv')


def yearVsAtt(teams):
    for team in teams:
        a = t.loc[t['Team'] == team]
        col = []
        for i in range(a.index[0], a.index[0] + len(a)):
            if a.loc[i]['Opponent_Rank'] == 'NR':
                col.append('blue')
            else:
                col.append('red')

        #x = sorted(a['Time'])
        x = sorted(a['Year'])
        y = list(a['Attendance'])
        xAry = np.array(x)
        yAry = np.array(y)
        m, b = np.polyfit(xAry, yAry, 1)
        #print(m, b)
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


if __name__ == '__main__':
    l = ['Penn State', 'Illinois', 'Rutgers', 'Wisconsin',
         'Michigan State', 'Northwestern', 'Indiana', 'Nebraska']
    yearVsAtt(l)

    # for i in l:
    # yearVsAtt(i)
    # test()

# -*- coding: utf-8 -*-
"""
Created on Sat Feb 23 11:06:01 2019

@author: thoma
"""


from __future__ import division

import pandas
from pandas import DataFrame
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from matplotlib.ticker import FuncFormatter
import numpy as np

# To choose color when doing graph, could put a list of colors in argument
def graph_builder_bar(graph, stacked):
    axes = graph.plot(
        kind = 'bar',
        stacked = stacked,
        )
    plt.axhline(0, color = 'k')
    axes.legend(
        bbox_to_anchor = (1.5, 1.05),
        )
    return plt.show()


def graph_builder_bar_percent(graph):
    axes = graph.plot(
        kind = 'bar',
        stacked = False,
        )
    plt.axhline(0, color = 'k')
    axes.yaxis.set_major_formatter(FuncFormatter(lambda y, _: '{:.1%}'.format(y)))
    # .1% means that we want one decimal
    axes.legend(
        bbox_to_anchor = (1.5, 1.05),
        )
    return plt.show()
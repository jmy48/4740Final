
# coding: utf-8

# In[35]:


"""
'Business'
                                       Importance
Preferred.Qualifications_skill 100.00
Preferred.Qualifications_busi 98.58
Preferred.Qualifications_experi 85.11
Minimum.Qualifications_experi 78.13
Responsibilities_intern 77.39

'DRTS'

Responsibilities_technic             100.00
Minimum.Qualifications_comput         77.66
Responsibilities_product              74.28
Responsibilities_solut                72.15
Minimum.Qualifications_scienc         72.13
Responsibilities_implement            65.19

'ED'

Responsibilities_design             100.00
Preferred.Qualifications_design      93.54
Preferred.Qualifications_abil        88.90
Minimum.Qualifications_scienc        88.12
Minimum.Qualifications_design        73.02
Responsibilities_user                71.75
pref_phd                               53.95


'IO'

Responsibilities_product              100.00
Preferred.Qualifications_abil          89.84
Minimum.Qualifications_year            84.54
Preferred.Qualifications_handl         77.13
Preferred.Qualifications_skill         76.69
Preferred.Qualifications_consult       74.11
Preferred.Qualifications_chang         69.49

'Marketing'
Preferred.Qualifications_market      100.00
Responsibilities_educ                 92.56
Responsibilities_market               88.49
Minimum.Qualifications_market         86.15
Responsibilities_partner              82.30
Preferred.Qualifications_execut       79.73

'OS'
Minimum.Qualifications_manag            100.00
Minimum.Qualifications_experi            75.29
Preferred.Qualifications_partner         58.94
min_years_exp                              56.42
Preferred.Qualifications_success         53.38
pref_bs                                    36.93


'Sales'
Responsibilities_custom                   100.00
Minimum.Qualifications_sale                95.37
Responsibilities_strateg                   75.60
Minimum.Qualifications_fluentli            74.39
Minimum.Qualifications_speak               74.21
Minimum.Qualifications_idiomat             73.62
Minimum.Qualifications_english             73.28

"""

d = """Minimum.Qualifications_manag            100.00
Minimum.Qualifications_experi            75.29
Preferred.Qualifications_partner         58.94
min_years_exp                              56.42
Preferred.Qualifications_success         53.38
pref_bs                                 36.93"""

features = []
importances = []
for line in d.split("\n"):
	tokens = [x for x in line.split(" ") if x]
	features.append(tokens[0])
	importances.append(float(tokens[1]))
#print(features, importances)

import plotly.plotly as py
import plotly.graph_objs as go

data = [go.Bar(
    y=features,
    x=importances,
    name=title,
    orientation = 'h',
     marker = dict(
        color = 'rgba(255, 255, 102, 1)')
)]

layout = go.Layout(
    title= title,
    yaxis= go.layout.YAxis(
        tickmode='array',
        automargin=True,
        titlefont=dict(size=30),
    ),
)

fig = go.Figure(data=data, layout=layout)
py.iplot(fig)


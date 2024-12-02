import networkx as nx
from scipy.linalg import expm, sinm, cosm
import numpy as np
from itertools import count
import matplotlib.pyplot as plt
import pandas as pd
import plotly.express as px
import osmnx as ox
from scipy import linalg
import matplotlib.patches as mpatches

# latlong, city = (41.390, 2.166), 'Barcelona'
# latlong, city = (33.755, -84.388), 'Atlanta'
city_list_full = ["São Paulo", "Rio de Janeiro", "Atlanta", "Manhattan", "Barcelona", "Madrid", "Buenos Aires", "London", "Beijing", "Paris", "Cardiff"]
lat_long_list_full = [(-23.546, -46.634), (-22.91100731359119, -43.20939643363555), (33.755, -84.388), (40.748, -73.985), (41.390, 2.166), (40.416, -3.703), (-34.609532137872215, -58.413562281598004), (51.51350693217751, -0.10620383552655813), (39.91410059119869, 116.39035428390193), (48.85968272013924, 2.343524958942639), (51.47684668368528, -3.1781309848863764)]
k = 10
latlong, city = lat_long_list_full[k], city_list_full[k]

dist = 4000
categories = ['motorway', 'motorway_link', 
              'trunk', 'trunk_link', 
              'primary', 'primary_link', 
              'secondary', 'secondary_link',
              'tertiary', 'tertiary_link',
              'residential', 
              'living_street', 'unclassified']

hwy_color = {'motorway': 'red',
             'motorway_link': 'firebrick',
             'trunk': 'orange',
             'trunk_link': 'orange',
             'primary': 'gold',
             'primary_link': 'olive',
             'secondary': 'green',
             'secondary_link' : 'green',
             'tertiary': 'green',
             'tertiary_link': 'green',
             'residential': 'blue',
             'crossing': 'k',
             'living_street': 'k',
             'unclassified': 'k',
             'closed': 'k',
             'busway': 'k',
             'disused':'k',
             'escape': 'k'}

# G = ox.graph_from_point(latlong, dist = dist, truncate_by_edge = True, network_type="drive")
G = ox.graph_from_place("Cardiff, UK", network_type="drive")

A = nx.to_numpy_array(G, dtype = float)
np.fill_diagonal(A, 0)
A = np.maximum(A, A.T)

comm = expm(A)
df_comm = pd.DataFrame(data = comm, columns = G.nodes(), index = G.nodes())
nodelist = G.nodes
diag = np.diag(df_comm)
nodelist = list(nodelist)
size = len(nodelist)
cd = np.zeros((size,size))

for i in range(size):
    for j in range(size):
        cd[i][j] = np.sqrt(diag[i] + diag[j] - 2*comm[i][j])

cd = pd.DataFrame(data = cd, index = nodelist, columns = nodelist)

dfA = nx.to_pandas_adjacency(G)
X = (dfA*cd)
GX = nx.from_pandas_adjacency(X, nx.DiGraph)

j = dict(nx.all_pairs_dijkstra_path_length(GX, weight = "weight"))
dfSP = nx.to_pandas_adjacency(G)


teste = pd.DataFrame.from_dict(j, orient = "index")
for i in teste.columns:
    for j in teste.columns:
        if np.isnan(teste[i][j]):
            dfSP[i][j] = 0
        else:
            dfSP[i][j] = teste[i][j]

ew_flow = dict()

for i in G.edges:
  ew_flow[(i[0], i[1], 0)] = dfSP[i[0]][i[1]]

nx.set_edge_attributes(G, ew_flow, "Cost of returnal")
nx.set_edge_attributes(G, nx.edge_betweenness_centrality(G, weight = "length"), "Edge Betweenness")

nodes, edges = ox.graph_to_gdfs(G)
nodes.to_csv(f"{city}_nodes.csv")
edges["highway"] = edges.highway.map(lambda x: x[0] if isinstance(x, list) else x)
edges.to_csv(f"{city}_edges.csv")

fig = px.box(edges, x = "highway", y = "Cost of returnal", title = city)
fig.update_xaxes(categoryorder='array', categoryarray=categories)
fig.write_image(f"{city}_box_classes.pdf")
fig.show()

teste = edges.map(lambda x: x[0] if isinstance(x, list) else x)
ec = teste.replace(hwy_color).highway
patches = []
for i in hwy_color:
    patches.append(mpatches.Patch(color= hwy_color[i], label=i))

fig, ax = ox.plot_graph(
    G, bgcolor="w", node_color='k', node_size=0, edge_linewidth=edges["Cost of returnal"]/max(edges["Cost of returnal"])*10, edge_color=ec, figsize = (20,20), show = False, close = False
)
ax.legend(handles = patches, bbox_to_anchor=(1.05, 1))
fig.savefig(f"{city}_COR_class.png", bbox_inches='tight')

fig = px.bar(edges.highway.value_counts(normalize=True), title = "Entire data set")
fig.update_xaxes(categoryorder='array', categoryarray=categories)
fig.update_layout(    font=dict(
        size=25,
        color="black"
    ))
fig.write_image(f"{city}_all_classes.pdf", width=1000, height=1000)

fig.show()

fig = px.bar(edges.sort_values(by="Cost of returnal")["highway"].head(len(edges)//100).value_counts(normalize=True), title = "Top 1% COR distribution")
fig.update_xaxes(categoryorder='array', categoryarray=categories)
fig.update_layout(    font=dict(
        size=25,
        color="black"
    ))
fig.write_image(f"{city}_1percent_classes.pdf", width=1000, height=1000)
fig.show()
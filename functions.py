def get_top_candidate(data):
    top_el = None
    top_val = 0
    for el in data['votes']:
        votes = int(el['votes'])
        if votes > top_val:
            top_val = votes
            top_el = el
    output = {}
    if top_el is None:
        print(f"{data['county_name']} {data['uat_name']}")
        raise ValueError()
    output['partid'] = top_el['party']
    output['localitate'] = data['uat_name']
    output['siruta'] = data['uat_siruta']
    output['judet'] = data['county_name']
    return output


def normalize(name, lowercase=False, strip_city=True):
    import unidecode
    if strip_city and (name[:5] == "ORAŞ " or name[:5] == "ORAS "):
        name = name[5:]
    if strip_city and name[:10] == "MUNICIPIUL":
        name = name[11:]
    if lowercase:
        name = name.lower()
    return unidecode.unidecode(name)
    

def get_middle_coords(place):
    lat = (place.bbox_north + place.bbox_south) / 2
    long = (place.bbox_east + place.bbox_west) / 2
    return lat[0], long[0]


def nearest_centers(lat, long, vacc_centers, ran=0.5):
    import osmnx as ox
    import numpy as np
    nearest = vacc_centers.loc[(vacc_centers['latitude'] > lat - ran) & 
                     (vacc_centers['latitude'] < lat + ran) &
                     (vacc_centers['longitude'] > long - ran) &
                     (vacc_centers['longitude'] < long + ran)
                    ]
    distances = []
    for i in range(nearest.shape[0]):
        distances.append(ox.great_circle_vec(lat1=lat, lng1=long, 
                            lat2=nearest.iloc[i]['latitude'], lng2=nearest.iloc[i]['longitude']))
    distances = np.sort(np.array(distances))
    if len(distances) == 0:
        return 100000, 0, 0
    return distances[0], len(distances), np.std(distances)
    

def elevations_from_response(response):
    return [x['elevation'] for x in response.json()['results']]


def get_elevations(data, start, finish, key):
    import requests
    locations = '|'.join(vacc_data.iloc[start:finish][['lat', 'long']]
         .apply(lambda x: f'{x[0]},{x[1]}', axis=1))
    url = f'https://maps.googleapis.com/maps/api/elevation/json?locations={locations}&key={key}'
    return requests.request("GET", url, headers={}, data={})


def add_county(data, step=3, start=3):
    data['judet'] = 'N/A'
    current_county = 'N/A'
    first = True
    for i in range(start, data.shape[0], step):
        if data.iloc[i]['uat'] == 'A. MUNICIPII SI ORASE' and first:
            current_county = data.iloc[i-1]['uat']
            for s in range(1, step + 1):
                if current_county == 'MUNICIPIUL BUCURESTI':
                    data.at[i-s, 'judet'] = current_county
                else:
                    data.at[i-s, 'judet'] = 'N/A'
            first = False
        for s in range(step):
            data.at[i + s, 'judet'] = current_county
        if data.iloc[i]['uat'] != 'A. MUNICIPII SI ORASE':
            first = True
    return data

def filter_rows(data):
    return data.drop(data[(data['sex'] != 'Ambele sexe') | 
         (data['uat']  == 'A. MUNICIPII SI ORASE') |
         (data['uat']  == 'B. COMUNE') |
         (data['judet']  == 'n/a') |
         (data['judet']  == 'N/A') | 
         (data['uat']  == 'ROMANIA')].index)
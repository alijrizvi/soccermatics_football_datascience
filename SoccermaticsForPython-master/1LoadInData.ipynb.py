#Load in Statsbomb competition and match data
#This is a library for loading json files.
import pandas as pd, json, requests

#Load the competition file
#Got this by searching 'how do I open json in Python'
with open('/Users/alijazibrizvi/Documents/Data Analytics/SoccermaticsInPython/open-data-master/data/competitions.json') as f:
    competitions = json.load(f)
        
# FIFA Mens World Cup 2018 has competition ID 43
competition_id = 43

/Users/alijazibrizvi/Documents/Data Analytics/SoccermaticsInPython/open-data-master/data/matches/43
# Loading in  the list of matches for this competition - the 2018 FIFA World Cup!
with open('/Users/alijazibrizvi/Documents/Data Analytics/SoccermaticsInPython/open-data-master/data/matches/' + str(competition_id)+'/3.json') as f:
    matches = json.load(f)

# Let's look inside the matches
matches[0]
matches[0]['home_team']
matches[0]['home_team']['home_team_name']
matches[0]['away_team']['away_team_name']

# Let's Print all Match Results
for match in matches:
    home_team_name=match['home_team']['home_team_name']
    away_team_name=match['away_team']['away_team_name']
    home_score=match['home_score']
    away_score=match['away_score']
    describe_text = 'The match between ' + home_team_name + ' and ' + away_team_name
    result_text = ' finished ' + str(home_score) +  ' : ' + str(away_score)
    print(describe_text + result_text)

# Now let's find a Match we are Interested in
home_team_required ="England"
away_team_required ="Panama"

# Find ID for the Match^
for match in matches:
    home_team_name=match['home_team']['home_team_name']
    away_team_name=match['away_team']['away_team_name']
    if (home_team_name==home_team_required) and (away_team_name==away_team_required):
        match_id_required = match['match_id']
print(home_team_required + ' vs ' + away_team_required + ' has id:' + str(match_id_required))
      

# Let's View as a DataFrame & Play Around with it
df = pd.read_json('/Users/alijazibrizvi/Documents/Data Analytics/SoccermaticsInPython/open-data-master/data/matches/43/3.json')

pd.set_option('display.max_columns', None)

df.head(8)







































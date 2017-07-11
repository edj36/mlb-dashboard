import urllib.request
import json
from datetime import date
import pandas as pd

def fix_digit(raw):
	"""fix_digitraw is a string representation of input [raw] (type int)
	that will never be a single digit (prepend 0 to fix)"""
	if raw < 10:
		return ("0" + str(raw))
	else:
		return str(raw)


def todays_games():
	"""prints out todays baseball games/scores"""
	url_str = "http://mlb.mlb.com/gdcross/components/game/mlb" \
    			+ "/year_" + fix_digit(date.today().year) \
    			+ "/month_" +  fix_digit(date.today().month) \
    			+ "/day_" + fix_digit(date.today().day) \
    			+ "/miniscoreboard.json"

	with urllib.request.urlopen(url_str) as url:
		data = json.loads(url.read().decode())
		games = data["data"]["games"]["game"]
		home = []
		home_score = []
		away = []
		away_score = []
		inning = []
		outs = []
		status = []
		for game in games:
			home.append(game.get("home_team_name", 0))
			home_score.append(game.get("home_team_runs", 0))
			away.append(game.get("away_team_name", 0))
			away_score.append(game.get("away_team_runs", 0))
			inning.append(game.get("inning", 0))
			outs.append(game.get("outs",0))
			status.append(game.get("status", 0))
		out = pd.DataFrame.from_items([("Home", home), \
							("Score", home_score), \
							("Away", away), \
							("Score", away_score), \
							("Inning", inning), \
							("Outs", outs), \
							("Status", status)])	
		print(out)	
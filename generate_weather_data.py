from bs4 import BeautifulSoup
import urllib3
import re
import os


def scrap_weather_data_into_file(month, year, place):

	"""
	Download json with historical weather data per month and year in place from timeanddate.com.
	Program creates subfolder called data_files if not exisits and populates it with data files.
	"""

	try:
		month = int(month)
	except ValueError:
		print('month not valid, cannot download data')
		return

	try:
		year = int(year)
	except ValueError:
		print('year not valid, cannot download data')
		return

	url = 'https://www.timeanddate.com/weather/poland/'+place+'/historic?month='+str(month)+'&year='+str(year)

	http = urllib3.PoolManager()
	r = http.request('GET', url)
	soup = BeautifulSoup(r.data, 'html.parser')
	element = soup.find(string=re.compile("var data"))

	directory = 'data_files'
	file_name = directory+'/'+place+'-'+str(year)+('0'+str(month) if month <10 else str(month))+'.json'

	if not os.path.exists(directory):
		try:
			os.makedirs(directory)
		except:
			print('something wrong with data files management')
			return

	with open(file_name, 'w+') as file:
		file.write(element[10:-34])

	print("Data in json downloaded and written down in : " + file_name)


def main():

	# scrap_weather_data_into_file(8, 2018, 'krakow')
	# scrap_weather_data_into_file(9, 2018, 'krakow')
	# scrap_weather_data_into_file(10, 2018, 'krakow')
	scrap_weather_data_into_file(11, 2018, 'krakow')


if __name__ == '__main__':
	main()
	
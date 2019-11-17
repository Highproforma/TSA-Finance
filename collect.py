import csv
import requests
import pandas as pd
from datetime import datetime
from abc import ABC, abstractmethod

class API(ABC):
    def __init__(self, base_URL=None):
        if base_URL is None:
            raise requests.exceptions.ConnectionError('No API base URL provided!')
        else:
            self.base_URL = base_URL
        super().__init__()
    
    def query(self, endpoint, payload = None):
        if payload is not None:
            response = requests.get(f'{self.base_URL}/{endpoint}', params=payload)
        else:
            response = requests.get(f'{self.base_URL}/{endpoint}')
        return response.status_code, response.json()
    
class CoinrankingAPI(API):
    def __init__(self, base_URL='https://api.coinranking.com/v1/public/', timeframe='24h'):
        assert timeframe in ['24h','7d','30d','1y','5y']
        super().__init__(base_URL)
        self.runtime = datetime.now().strftime('%s')
        self.timeframe = timeframe

    def query(self, endpoint, payload=None):
        return super().query(endpoint, payload=payload)
        
    def get_coins(self):
        print('Fetching coins', end='')
        with open('./data/coins.csv', mode='w+') as coinsFH:
            coins_writer = csv.writer(coinsFH, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            coins_writer.writerow(['id', 'name', 'symbol', 'slug'])
            payload = {'base': 'USD', 'limit': 100, 'offset': 0}
            status, response = self.query(endpoint='coins',payload=payload)
            while status == 200 and response['data']['stats']['total'] > (response['data']['stats']['offset'] + response['data']['stats']['limit']):
                print('.', end='')
                for coin in response['data']['coins']:
                    coins_writer.writerow([coin['id'], coin['name'], coin['symbol'], coin['slug']])
                payload['offset'] += payload['limit']
                status, response = self.query(endpoint='coins',payload=payload)
        print('Done!')

    def get_market_data(self, coin):
        print('Fetching coin data ({}) for coin {} - {}'.format(self.timeframe, coin.id, coin.symbol))
        with open('./data/{}_{}_{}.csv'.format(coin.symbol, self.runtime, self.timeframe), mode='w+') as coinsFH:
            coins_writer = csv.writer(coinsFH, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            coins_writer.writerow(['timestamp', 'price'])
            payload = {'base': 'USD'}
            status, response = self.query(endpoint='coin/{}/history/{}'.format(coin.id, self.timeframe), payload=payload)
            for info in response['data']['history']:
                coins_writer.writerow([info['timestamp'], info['price']])

if __name__ == "__main__":
    api = CoinrankingAPI(timeframe='24h')
    api.get_coins()
    coinsDF = pd.read_csv('./data/coins.csv', sep=';', header=0)
    #(coinsDF[coinsDF['symbol'].isin(['BTC', 'ETH', 'BCH'])][['id','symbol']]).apply(api.get_market_data, axis=1)
    (coinsDF[['id','symbol']]).apply(api.get_market_data, axis=1)
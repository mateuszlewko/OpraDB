g.V().has('code', 'WRO').out('route').path().by('code')

g.V().has('airport','country', 'PL').as('pl').out('route') \
     .has('airport', 'city', 'Munich').out('route').has('country', 'DE') \
     .as('de').select('pl', 'de').by('code').by('code')
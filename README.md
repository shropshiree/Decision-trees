# Opis

Projekt ten to praca licencjacka porównująca skuteczność drzew decyzyjnych na danych o problematyce medycznej. Badanie zostało przeprowadzone w języku programowania R.

## Streszczenie

Niniejsza praca licencjacka przedstawia zastosowanie drzew decyzyjnych w problematyce medycznej. Celem pracy było skonstruowanie możliwie najlepszego drzewa decyzyjnego oraz zbadanie, które z wybranych przeze mnie czynników istotnie wpływają na wskaźnik śmiertelności noworodków. W pracy użyłam danych pochodzących z World Health Organization z 2015 roku. Utworzony przeze mnie zbiór danych zawiera 194 obserwacji, każda z nich to inny kraj. Poza zmienną objaśnianą wybrałam 9 zmiennych objaśniających. Do weryfikacji, który z powstałych modeli jest najlepszy użyłam dwóch miar błędu: pierwiastka błędu średniokwadratowego oraz średniego bezwzględnego błędu procentowego. 
  
  
W niniejszej pracy skonstruowałam pojedyncze drzewo decyzyjne oraz modele stworzone za pomocą metod boosting, bagging oraz lasów losowych. Najlepszą z nich okazała się być metoda boosting. Mimo tego wynik nie był w pełni satysfakcjonujący, co może być związane z faktem, iż śmiertelność ma w sobie losowość. Największy wpływ na wskaźnik śmiertelności noworodków okazały się mieć wielkość bieżących wydatków zdrowotnych przypadających na jednego mieszkańca oraz procent społeczeństwa korzystających przynajmniej z podstawowych usług sanitarnych. 

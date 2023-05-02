lore CAPEPERIL => area 'Cape Peril';

translations pl => {
	name => "Przylądek Peril",
	desc => "Legenda głosi, że pierwsi odkrywcy, którzy postawili tu stopę po wielomiesięcznej żegludze, mieli nazwać to miejsce Przylądkiem Wybawienia. Pozornie bezpieczna kraina nie miała im jednak nic do zaoferowania prócz śmierci. Dziesięciolecia później wciąż opiera się kolonizatorom, karając ich tym okrutniej, im bardziej zuchwałe są ich zapędy."
};

### LOCATION

lore CP_HARBOR => location 'Cape Peril - Harbor';

parent area 'Cape Peril';

translations pl => {
	name => "Port Peril",
	desc => "Postawiony w okolicy przylądka Peril port to zazwyczaj pierwsze miejsce, w którym lądują nowo przybyli osadnicy, kupcy oraz poszukiwacze przygód."
};

specify map => 'peril_harbor';

### LOCATION

# lore CP_MARKET => location 'Cape Peril - Market';

# parent area 'Cape Peril';

# translations pl => {
# 	name => "Rynek",
# 	desc => "Wąskie, kręte uliczki w połowie zastawione są stoiskami oferującymi wszelakie produkty. Wszechobecny tłok i gwar skutecznie ukrywają toczone tutaj czarne interesy przed skorumpowaną milicją."
# };

# specify map => 'peril_market.tmx';

### CONNECTIONS AND METADATA

# connection
# 	location 'Cape Peril - Market',
# 	location 'Cape Peril - Harbor';

# load coordinates from cape_peril.json
load_coordinates 'Markers';

# desc => "Brama w palisadzie wyznacza koniec obszaru objętego ochroną milicyjną. Patrole ograniczają się tylko do sprawdzania stanu ogrodzenia oraz bezpośredniego sąsiedztwa bramy. Głucha cisza na zewnątrz mocno kontrastuje z hałasem dobiegającym ze środka."
# desc => "Biorąc pod uwagę czas trwania rejsu nie powinno dziwić, że jest to pierwsze miejsce, do którego udają się nowo przybyli. Mimo oczywistej pokusy widać tu niewielu marynarzy, którzy często wolą nie opuszczać okrętów poza koniecznością."
# desc => "Prawdopodobnie najbardziej chroniony budynek w całym porcie. Zaufana instytucja odpłatnie przetrzymująca towary oraz kosztowności od każdego, kto zapłaci."


use v5.32;
use warnings;

use Test::More;

use_ok('Game::Mechanics::Distance');

ok Game::Mechanics::Distance->is_in_range([0, 0], [4, 3], 5.0), 'valid range ok';
ok Game::Mechanics::Distance->is_in_range([4, 3], [0, 0], 5.0), 'valid range ok';
ok !Game::Mechanics::Distance->is_in_range([0, 0], [4, 3], 4.9), 'invalid range ok';

done_testing;

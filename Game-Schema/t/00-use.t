use v5.30; use warnings;

use Test::More;

use_ok('Game::Schema');
#
# use a model to see if it registers
use_ok('Game::Model::Player');

done_testing;

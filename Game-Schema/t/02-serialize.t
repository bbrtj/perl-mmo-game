use v5.30;
use warnings;

use Test::More;
use Game::Model::CharacterVariables;

my $data = {
	experience => 100000,
	location => "TEST",
	health => 100,
	focus => 30,
};

my $vars = Game::Model::CharacterVariables->new($data);
$data->{id} = $vars->id;
is_deeply $vars->serialize, $data, 'serialization ok';

done_testing;


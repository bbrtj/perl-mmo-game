use Game::Model::CharacterVariables;

use testheader;

my $data = {
	experience => 100000,
	location => "TEST",
	health => 100,
	mana => 30,
};

my $vars = Game::Model::CharacterVariables->new($data);
$data->{id} = $vars->id;
is $vars->serialize, $data, 'serialization ok';

done_testing;


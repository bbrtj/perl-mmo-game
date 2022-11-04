use all 'Model';

use testheader;

my $data = {
	experience => 100000,
	location_id => "TEST",
	health => 100,
	energy => 30,
	pos_x => 0,
	pos_y => 0,
};

my $vars = Model::CharacterVariables->new($data);
$data->{id} = $vars->id;
is $vars->serialize, $data, 'serialization ok';

done_testing;


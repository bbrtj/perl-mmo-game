use v5.30;
use warnings;

use Test::More;
use Game::Ability::Parser;
use Game::Common::Container;
use Game::Model::CharacterCache;
use lib 't/lib';
use DatabaseTest;

DatabaseTest->test(
	sub {
		# uses Game::Repository::AbilityData
		my $parsed = Game::Ability::Parser->parse;
		ok exists $parsed->{ABI_STRIKE},
			'ability got compiled';

		my $ability = $parsed->{ABI_STRIKE};
		isa_ok $ability, 'Game::Ability::Compiled',
			'ability is a compiled class';
		is scalar $ability->effect_table->@*, 1,
			'ability has a single group';
		is scalar $ability->effect_table->[0]->@*, 2,
			'ability has two effects';

		# test Game::Repository::CharCache
		my $char_repo = resolve('repo')->char_cache;
		ok $char_repo, 'character cache repo resolve ok';

		my $data = {
			level => 5,
			health_max => 300,
			health_regen => 1.5,
			focus_max => 150,
			focus_regen => 6,
			stats => 'STT_STR:30;STT_AGI:19;STT_INT:10;STT_STA:25',
		};

		my $model = Game::Model::CharacterCache->new($data);
		$data->{id} = $model->id;
		$char_repo->save($model->serialize);

		my $loaded = $char_repo->load($model->id);
		is_deeply $loaded, $data, 'char cache repo save-load ok';

		$data->{health_max} += 5;
		ok $char_repo->save($data), 'update ok';
	}
);

done_testing;

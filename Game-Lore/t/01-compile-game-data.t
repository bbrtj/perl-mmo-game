use v5.32;
use warnings;

use Test::More;
use Mojo::Collection;
use Game::Common::Container qw(add_to_container);
use Game::Ability;
use Game::Character::Class;
use Game::RepositoryBase;

package AbilityRepoMock {
	use Moo;

	with 'Game::Repository::Role::Resource';

	sub save { ... }

	sub load
	{
		return [
			{
				id => 'test',
				attribute => 'ABA_ELEM',
				instant => 0,
				cost => 0,
				cooldown => 5,
				range => 50,
				target_self => 0,
				target_ally => 0,
				target_foe => 0,
				target_ground => 1,
				effect_type => 'AET_HEAL',
				effect_attribute => 'ABA_VVV',
				effect_group => 1,
				value => 50,
				deviation => 3,
			},
			{
				id => 'test',
				attribute => 'ABA_ELEM',
				instant => 0,
				cost => 0,
				cooldown => 5,
				range => 50,
				target_self => 0,
				target_ally => 0,
				target_foe => 0,
				target_ground => 1,
				effect_type => 'AET_COEF_INT',
				effect_attribute => 'ABA_VVV',
				effect_group => 1,
				value => 0.2,
				deviation => undef,
			},
			{
				id => 'test',
				attribute => 'ABA_ELEM',
				instant => 0,
				cost => 0,
				cooldown => 5,
				range => 50,
				target_self => 0,
				target_ally => 0,
				target_foe => 0,
				target_ground => 1,
				effect_type => 'AET_DURAT',
				effect_attribute => 'ABA_VVV',
				effect_group => 1,
				value => 3,
				deviation => undef,
			}
		];
	}
};

package ClassRepoMock {
	use Moo;

	with 'Game::Repository::Role::Resource';

	sub save { ... }

	sub load
	{
		return [
			{
				id => 'class1',
				playable => 1,
				base_health => 100,
				health_per_level => 100,
				base_health_regen => 100,
				health_regen_per_level => 100,
				base_mana => 1,
				mana_per_level => 100,
				base_mana_regen => 100.1,
				mana_regen_per_level => 100,
				base_stats => 'STT_INT:50',
				stats_per_level => 'STT_INT:20',
				ability => 'test',
			},
		];
	}
};

add_to_container(
	repo => Game::RepositoryBase->new(
		ability_data => AbilityRepoMock->new,
		class_data => ClassRepoMock->new,
	)
);

ABILITIES: {
	my $parsed = Game::Ability->get;

	ok exists $parsed->{test};
	is scalar keys $parsed->%*, 1;

	my $subject = $parsed->{test};

	isa_ok $subject, "Game::Ability::Compiled";
	isa_ok $subject->attribute, "Game::Ability::Attribute::Elemental";
	ok !$subject->instant;
	is $subject->cost, 0;
	is $subject->cooldown, 5;
	is $subject->range, 50;
	ok !$subject->target_self;
	ok !$subject->target_ally;
	ok !$subject->target_foe;
	ok $subject->target_ground;

	ok exists $subject->groups->{1};
	is scalar keys $subject->groups->%*, 1;

	$subject = $subject->group(1);
	isa_ok $subject, "Game::Ability::Compiled::Group";
	ok exists $subject->effects->[0];
	is scalar $subject->effects->@*, 3;

	for my $index (0 .. 2) {
		my $subject = $subject->effects->[$index];
		isa_ok $subject, "Game::Ability::Compiled::Effect";
		if ($subject->effect_type->isa("Game::Ability::EffectType::Healing")) {
			isa_ok $subject->attribute, "Game::Ability::Attribute::Inherit";
			is $subject->value, 50;
			is $subject->deviation, 3;
		}
		elsif ($subject->effect_type->isa("Game::Ability::EffectType::IntellectCoefficient")) {
			is $subject->value, "0.2";
			ok !defined $subject->deviation;
		}
		elsif ($subject->effect_type->isa("Game::Ability::EffectType::Duration")) {
			is $subject->value, 3;
			ok !defined $subject->deviation;
		}
		else {
			fail "Unknown effect type";
		}
	}
}

CLASSES: {
	my $parsed = Game::Character::Class->get;

	ok exists $parsed->{class1};
	is scalar keys $parsed->%*, 1;

	my $subject = $parsed->{class1};

	isa_ok $subject, "Game::Character::Class::Compiled";
	is $subject->playable, 1, 'playable ok';
	is $subject->base_mana_regen, '100.1', 'mana regen ok';

	$subject = $subject->abilities;
	is scalar $subject->@*, 1, 'class ability count ok';

	$subject = $subject->[0];
	isa_ok $subject, 'Game::Ability::Compiled', 'ability isa ok';
	is $subject->lore_id, 'test', 'ability id ok';
}

done_testing;

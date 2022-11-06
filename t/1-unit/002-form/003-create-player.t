use Form::CreatePlayer;
use Utils;
use Game::Helpers;

use testheader;

test_data
	'validation should succeed' => [
		[{name => 'test', class => lore_class('Witchhunter')->id}],
	];

test_data
	'validation should fail' => [
		[
			{},
			{'name' => ['field is required'], 'class' => ['field is required']}
		],
		[
			{name => 'aa', class => lore_class('Witchhunter')->id},
			{'name' => ['err.name_too_short[]|3']}
		],
		[
			{name => 'aaaaaaaaaaaaaaaaaaaaa', class => lore_class('Witchhunter')->id},
			{'name' => ['err.name_too_long[]|20']}
		],
		[
			{name => 'aaa5', class => lore_class('Witchhunter')->id},
			{'name' => ['err.name_must_consist_of_letters']}
		],
		[
			{name => 'aaa aaa', class => lore_class('Witchhunter')->id},
			{'name' => ['err.name_must_consist_of_letters']}
		],
		[
			{name => 'aaaaa', class => 'not an id'},
			{'class' => ['err.element_invalid']}
		],
		[
			{name => 'aaaaa', class => lore_attribute('Physical')->id},
			{'class' => ['err.element_invalid']}
		],
	];

test validation_should_succeed => sub ($data) {
	my $form = Form::CreatePlayer->new;
	$form->set_input($data);
	ok $form->valid, "form valid $_";
};

test validation_should_fail => sub ($data, $errors) {
	my $form = Form::CreatePlayer->new;
	$form->set_input($data);
	ok !$form->valid, "form invalid $_";
	is $form->errors_hash, $errors, "errors hash $_";
};

done_testing;


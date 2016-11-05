import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class Hannan {

	private HashMap<String, Person> mapOfPeople = new HashMap<String, Person>();

	public static void main(String[] args) throws FormatException, IOException {
		Scanner scan = new Scanner(System.in);

		Hannan h = new Hannan();

		while (scan.hasNext()) {
			String line = scan.nextLine();

			System.out.println(line);
			System.out.println(h.processLine(line));
		}

		scan.close();
	}

	protected String processLine(String lineFromFile) throws FormatException {
		String[] lineSegments = lineFromFile.split(" ");
		String type = lineSegments[0];
		String name1 = lineSegments[1];
		String name2 = lineSegments[2];

		// Might have a third name
		String name3 = null;
		if (lineSegments.length > 3)
			name3 = lineSegments[3];

		switch (type.toUpperCase()) {

		case "E":
			// Add people to the family tree
			return eventProcessor(name1, name2, name3);
		case "R":
			// Get closest relationship
			return closestRelationship(name1, name2);
		case "X":
			// Get if relationship is there
			return relationshipExists(name1, name2, name3);
		case "W":
			// Get list of people related
			return listOfRelationship(name1, name2);
		default:
			throw new FormatException("Error Parsing Line");

		}
	}

	protected String listOfRelationship(String name1, String name2) {
		String relationship = name1.toUpperCase();
		Person person = mapOfPeople.get(name2);

		if (person == null)
			return "";

		switch (relationship) {
		case "SPOUSE":
			return person.getSpouse().stream().filter((p) -> p != null).sorted().map((p) -> p.name)
					.collect(Collectors.joining(System.lineSeparator()));
		case "PARENT":
			return person.getParents().stream().filter((p) -> p != null).map((p) -> p.name)
					.collect(Collectors.joining(System.lineSeparator()));
		case "SIBLING":
			return person.getSiblings().stream().filter((p) -> p != null).map((p) -> p.name)
					.collect(Collectors.joining(System.lineSeparator()));
		case "HALF-SIBLING":
			return person.getHalfSiblings().stream().filter((p) -> p != null).map((p) -> p.name)
					.collect(Collectors.joining(System.lineSeparator()));
		case "ANCESTOR":
			return person.getAncestors().stream().filter((p) -> p != null).map((p) -> p.name)
					.collect(Collectors.joining(System.lineSeparator()));
		case "COUSIN":
			return person.getCousins().stream().filter((p) -> p != null).map((p) -> p.name)
					.collect(Collectors.joining(System.lineSeparator()));
		case "UNRELATED":
			TreeSet<Person> allRelated = person.anyRelation();
			return mapOfPeople.values().stream().sorted().filter((p) -> p != null)
					.filter((p) -> !allRelated.contains(p)).map((p) -> p.name)
					.collect(Collectors.joining(System.lineSeparator()));

		default:
			return "";
		}
	}

	protected String relationshipExists(String name1, String name2, String name3) {
		Person person1 = mapOfPeople.get(name1);
		Person person2 = mapOfPeople.get(name3);
		String relationship = name2.toUpperCase();

		if (person1 == null || person2 == null)
			return "No";

		switch (relationship) {
		case "SPOUSE":
			if (person1.getSpouse().equals(person2))
				return "Yes";
			else
				return "No";

		case "PARENT":
			if (person1.getParents().contains(person2) || person1.children.contains(person2))
				return "Yes";
			else
				return "No";

		case "SIBLING":
			if (person1.getSiblings().contains(person2))
				return "Yes";
			else
				return "No";

		case "HALF-SIBLING":
			if (person1.getHalfSiblings().contains(person2))
				return "Yes";
			else
				return "No";

		case "ANCESTOR":
			if (person1.getAncestors().contains(person2))
				return "Yes";
			else
				return "No";

		case "COUSIN":
			if (person1.getCousins().contains(person2))
				return "Yes";
			else
				return "No";

		case "UNRELATED":
			if (!person1.anyRelation().contains(person2))
				return "Yes";
			else
				return "No";

		default:
			return "No";
		}
	}

	protected String closestRelationship(String name1, String name2) {
		Person person1 = mapOfPeople.get(name1);
		Person person2 = mapOfPeople.get(name2);
		// If either person is null then unrelated
		if (person1 == null || person2 == null)
			return "Unrelated";

		if (person1.getSpouse().contains(person2))
			return "Spouse";

		if (person1.getParents().contains(person2) || person1.children.contains(person2))
			return "Parent";

		if (person1.getSiblings().contains(person2))
			return "Sibling";

		if (person1.getHalfSiblings().contains(person2))
			return "Half-Sibling";

		if (person1.getAncestors().contains(person2))
			return "Ancestor";

		if (person1.getCousins().contains(person2))
			return "Cousin";

		return "Unrelated";
	}

	protected String eventProcessor(String name1, String name2, String childName) {
		Person person1 = mapOfPeople.get(name1); // Get Person 1
		if (person1 == null) { // If Person1 is not in map then add them
			person1 = new Person(name1);
			mapOfPeople.put(name1, person1);
		}

		Person person2 = mapOfPeople.get(name2); // Get Person 2
		if (person2 == null) { // If person2 is not in map then add them
			person2 = new Person(name2);
			mapOfPeople.put(name2, person2);
		}

		// Set people to each others spouses
		if (!person1.getSpouse().contains(person2))
			person1.marry(person2);
		if (!person2.getSpouse().contains(person1))
			person2.marry(person1);

		// If there is a child, add them
		if (childName != null) {
			// The child will never have been in the list
			Person child = new Person(childName, person1, person2, null, null);
			mapOfPeople.put(childName, child);
			person1.birth(child);
			person2.birth(child);
		}

		return name1 + " married " + name2 + (childName != null ? " and had " + childName : "");
	}

	private class Person implements Comparable<Person> {
		private String name; // Name of Person
		private Person[] parents; // Parents of person (up to 2)
		private List<Person> spouse; // Spouse of Person, can be null
		private List<Person> children; // Children of the person

		protected Person(String name, Person parent1, Person parent2, Person spouse, Person... children) {
			this.name = name;
			this.parents = new Person[2];
			this.parents[0] = parent1;
			this.parents[1] = parent2;
			this.spouse = new ArrayList<Person>();
			if (spouse != null)
				this.spouse.add(spouse);
			this.children = new ArrayList<>();
			if (children != null)
				for (int i = 0; i < children.length; i++) {
					this.children.add(children[i]);
				}
		}

		protected Person(String name) {
			this.name = name;
			this.parents = new Person[2];
			this.spouse = new ArrayList<Person>();
			this.children = new ArrayList<Person>();
		}

		protected void marry(Person p) {
			spouse.add(p);
		}

		protected void birth(Person child) {
			children.add(child);
		}

		protected List<Person> getSpouse() {
			return spouse;
		}

		protected TreeSet<Person> getParents() {
			TreeSet<Person> tree = new TreeSet<Person>();
			if (parents[0] != null)
				tree.add(parents[0]);
			if (parents[1] != null)
				tree.add(parents[1]);
			return tree;
		}

		protected TreeSet<Person> getSiblings() {
			TreeSet<Person> tree = new TreeSet<Person>(); // Using TreeSet so
															// cant have dups

			// If both parents match then add to tree
			if (parents[0] != null)
				parents[0].children.stream().filter((child) -> !child.equals(this)).filter(
						(child) -> child.getParents().contains(parents[0]) && child.getParents().contains(parents[1]))
						.forEach((child) -> tree.add(child));

			// If both parents match then add to tree
			if (parents[1] != null)
				parents[1].children.stream().filter((child) -> !child.equals(this)).filter(
						(child) -> child.getParents().contains(parents[0]) && child.getParents().contains(parents[1]))
						.forEach((child) -> tree.add(child));

			return tree;
		}

		protected TreeSet<Person> getHalfSiblings() {
			TreeSet<Person> tree = new TreeSet<Person>(); // Using TreeSet so
															// can't have dups

			// If only a single parent is shared then they are half siblings
			if (parents[0] != null)
				parents[0].children.stream().filter((child) -> !child.equals(this)).filter(
						(child) -> (child.getParents().contains(parents[0]) && !child.getParents().contains(parents[1]))
								|| (!child.getParents().contains(parents[0])
										&& child.getParents().contains(parents[1])))
						.forEach((child) -> tree.add(child));

			// If only a single parent is shared then they are half siblings
			if (parents[1] != null)
				parents[1].children.stream().filter((child) -> !child.equals(this)).filter(
						(child) -> (child.getParents().contains(parents[0]) && !child.getParents().contains(parents[1]))
								|| (!child.getParents().contains(parents[0])
										&& child.getParents().contains(parents[1])))
						.forEach((child) -> tree.add(child));

			return tree;
		}

		protected TreeSet<Person> getAncestors() {

			TreeSet<Person> tree = new TreeSet<Person>();
			tree.addAll(getParents());

			if (parents[0] == null)
				return tree;

			return combineTreeSets(tree, parents[0].getAncestors(), parents[1].getAncestors());
		}

		protected TreeSet<Person> getDescendants() {
			TreeSet<Person> tree = new TreeSet<Person>();

			if (children.isEmpty())
				return tree;

			children.stream().forEach((child) -> {
				tree.add(child);
				tree.addAll(child.getDescendants());
			});

			return tree;
		}

		protected TreeSet<Person> getCousins() {
			TreeSet<Person> tree = new TreeSet<Person>();
			TreeSet<Person> ancestors = getAncestors();

			ancestors.stream().forEach((ancestor) -> {
				TreeSet<Person> ancestorSiblings = ancestor.getSiblings();
				ancestorSiblings.stream().forEach((ancestorSibling) -> {
					tree.add(ancestorSibling);
					tree.addAll(ancestorSibling.getDescendants());
				});
			});

			TreeSet<Person> siblings = getSiblings();
			siblings.stream().forEach((sibling) -> tree.addAll(sibling.getDescendants()));

			return tree;
		}

		protected TreeSet<Person> anyRelation() {
			TreeSet<Person> tree = new TreeSet<Person>();
			tree.add(this);
			tree.addAll(getSpouse());
			tree.addAll(getParents());
			tree.addAll(children);
			return combineTreeSets(tree, getCousins(), getAncestors(), getHalfSiblings(), getSiblings(),
					getDescendants());
		}

		@Override
		public boolean equals(Object p) {
			Person person = (Person) p;
			if (this == null || p == null)
				return false;
			return name.equalsIgnoreCase(person.name);
		}

		@Override
		public int compareTo(Person p) {
			return this.name.compareTo(p.name);
		}
	}

	private class FormatException extends Exception {

		public FormatException(String string) {
			super(string);
		}

		/**
		 * 
		 */
		private static final long serialVersionUID = -5259528542532991354L;
	}

	private TreeSet<Person> combineTreeSets(TreeSet<Person>... trees) {
		TreeSet<Person> resultTree = new TreeSet<Person>();
		for (TreeSet<Person> tree : trees)
			resultTree.addAll(tree);
		return resultTree;
	}

}

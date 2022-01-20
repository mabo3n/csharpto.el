using Xunit;
using FluentAssertions;
using System;
using Company.Product.Domain.Entities;

namespace Company.Product.Tests.Domain.Entities
{
    public class OriginTests
    {
        [Theory]
        [InlineDataAttribute(null)]
        [InlineDataAttribute("")]
        [InlineDataAttribute(
            " "
        )]
        public void Constructor_ShouldThrowArgumentException_WhenNameIsEmpty(
            string name
        )
            => Record.Exception(
                () => new Origin(name, Guid.NewGuid())
            )
                .Should()
                .NotBeNull()
                .And
                .BeOfType<ArgumentException>();

        [Fact] public void ChangeName_ShouldChangeName() {
            var oldName = "PDCA  ";
            var origin = new Origin(oldName, Guid.NewGuid());

            var newName = "PDCA";
            origin.ChangeName(newName);

            origin.Name
                .Should()
                .Be(newName);
        }
    }
}

extern crate svg_util;
use svg_util::primitive;

#[test]
fn test_optimizing() {
    let (primitives, err, precision) = primitive::parse_path("M 100 100 L 101 101");
    assert_eq!(err, None);
    assert_eq!(precision, 0);
    
    let mut str = String::new();
    let res = primitive::write_path(&mut str, &primitives, false, None);
    assert!(res.is_ok());
    assert_eq!(str, "M100 100l1 1");
}

#[test]
fn test_horizontal_vertical() {
    // Vertical
    let (primitives, err, precision) = primitive::parse_path("M 100 100 L 100 101");
    assert_eq!(err, None);
    assert_eq!(precision, 0);
    
    let mut str = String::new();
    let res = primitive::write_path(&mut str, &primitives, false, None);
    assert!(res.is_ok());
    assert_eq!(str, "M100 100v1");

    // Horizontal
    let (primitives, err, precision) = primitive::parse_path("M 100 100 L 101 100");
    assert_eq!(err, None);
    assert_eq!(precision, 0);
    
    let mut str = String::new();
    let res = primitive::write_path(&mut str, &primitives, false, None);
    assert!(res.is_ok());
    assert_eq!(str, "M100 100h1");

    // Vertical with deviation smaller than precision
    let (primitives, err, precision) = primitive::parse_path("M 100 100 L 100.1 101");
    assert_eq!(err, None);
    assert_eq!(precision, 1);
    
    let mut str = String::new();
    let res = primitive::write_path(&mut str, &primitives, false, Some(0));
    assert!(res.is_ok());
    assert_eq!(str, "M100 100v1");
}